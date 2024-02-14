{-# LANGUAGE BangPatterns
  #-}

module Sorts.Simple3Way (sort, sortBy) where

sort :: Ord a => [a] -> [a]
sort = sortBy compare

sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy cmp = mergeAll . sequences
  where
    x `gt` y = x `cmp` y == GT

    sequences (a:b:xs)
        | a `gt` b  = descending b [a]  xs
        | otherwise = ascending  b (a:) xs
    sequences xs  = [xs]

    descending a as (b:bs)
        | a `gt` b
        = descending b (a:as) bs
    descending a as bs =
        (a : as) : sequences bs

    ascending a as (b:bs)
        | not (a `gt` b)
        = ascending b (\ys -> as (a:ys)) bs
    ascending a as bs =
      let !x = as [a]
      in x : sequences bs

    mergeAll [x] = x
    mergeAll xs  = mergeAll (mergePairs xs)

    mergePairs (a : b : c : xs) =
      let !x = merge3 a b c
      in x : mergePairs xs
    mergePairs [a, b] = [merge a b]
    mergePairs xs     = xs

    merge as@(a : as') bs@(b : bs')
        | a `gt` b  = b : merge as  bs'
        | otherwise = a : merge as' bs
    merge [] bs     =               bs
    merge as []     =           as

    -- `merge3` is a manually fused version of `merge (merge as bs) cs`
    merge3 as@(a : as') bs@(b : bs') cs
        | a `gt` b  = merge3X b as  bs' cs
        | otherwise = merge3X a as' bs  cs
    merge3 [] bs cs = merge         bs  cs
    merge3 as [] cs = merge     as      cs

    merge3X x as bs cs@(c:cs')
        | x `gt` c     = c : merge3X x as bs cs'
        | otherwise    = x : merge3    as bs cs
    merge3X x as bs [] = x : merge     as bs

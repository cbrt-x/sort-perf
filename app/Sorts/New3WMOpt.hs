module Sorts.New3WMOpt (sort, sortBy) where
import Data.List.NonEmpty (NonEmpty(..))
import Test.Tasty.QuickCheck (NonEmptyList(NonEmpty))
import Debug.Trace (traceShow, traceShowId, trace)
import Unsafe.Coerce (unsafeCoerce)

sort :: Ord a => [a] -> [a]
sort = sortBy compare

sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy _ [] = []
sortBy _ [x] = [x]
sortBy cmp ns
  | [x, y]    <- ns = merge [x] [y]
  | [x, y, z] <- ns = merge' [x] [y] [z]
  | otherwise       = mergeAll (sequences ns)
  where
    x `gt` y = x `cmp` y == GT

    sequences (a:b:xs)
      | a `gt` b  = descending b [a]  xs
      | otherwise = ascending  b (a:) xs
    sequences xs = [xs]

    descending a as (b:bs)
      | a `gt` b       = descending b (a:as) bs
    descending a as bs = (a:as): sequences bs

    ascending a as (b:bs)
      | not (a `gt` b) = ascending b (\ys -> as (a:ys)) bs
    ascending a as bs  = let !x = as [a]
                         in x : sequences bs

    mergeAll [x] = x
    mergeAll xs  = mergeAll (mergePairs xs)

    mergePairs [a, b, c]  = [merge' a b c]
    mergePairs (a:b:c:xs) = let !x = merge' a b c
                            in x : mergePairs xs
    mergePairs [a,b]      = [merge a b]
    mergePairs xs         = xs


    merge as@(a:as') bs@(b:bs')
      | a `gt` b   = b : merge as  bs'
      | otherwise  = a : merge as' bs
    merge [] bs   = bs
    merge as []   = as


    merge' (a:as) (b:bs) (c:cs)
      = let (a1, b1) = min' (a:|as, LT) (b:|bs, EQ)
            (b2, cf) = min' b1 (c:|cs, GT)
            (af, bf) = min' a1 b2
        in merge's af bf cf
    merge' [] bs cs = merge bs cs
    merge' as [] cs = merge as cs
    merge' as bs [] = merge as bs

    min' as@(a:|_, _) bs@(b:|_, _)
      | a `gt` b  = (bs, as)
      | otherwise = (as, bs)

    merge's (a:|(a':as'), ac) bs@(b:|_, bc) cs@(c:|_, cc) = a : rec (a':|as', ac)
      where rec as
              | a_cmp_b == LT          = merge's as bs cs
              | a_cmp_b == EQ, ac < bc = merge's as bs cs
              | a_cmp_c == LT          = merge's bs as cs
              | a_cmp_c == EQ, ac < cc = merge's bs as cs
              | otherwise      = merge's bs cs as
              where a_cmp_b = a' `cmp` b
                    a_cmp_c = a' `cmp` c
    merge's (a:|[], _) (b:|bs, bc) (c:|cs, cc)
      | bc < cc = a : merge (b:bs) (c:cs)
      | otherwise = a : merge (c:cs) (b:bs)

    {- unstable
    merge' (a:as) (b:bs) (c:cs)
      = let (a1, b1) = min' (a:|as) (b:|bs)
            (b2, cf) = min' b1 (c:|cs)
            (af, bf) = min' a1 b2
        in merge's af bf cf
    merge' [] bs cs = merge bs cs
    merge' as [] cs = merge as cs
    merge' as bs [] = merge as bs

    min' as@(a:|_) bs@(b:|_)
      | a `gt` b  = (bs, as)
      | otherwise = (as, bs)


    merge's (a:|(a':as')) bs@(b:|_) cs@(c:|_) = a : rec (a':|as')
      where rec as | not (a' `gt` b) = merge's as bs cs
                   | not (a' `gt` c) = merge's bs as cs
                   | otherwise = merge's bs cs as
    merge's (a:|[]) (b:|bs) (c:|cs) = a : merge (b:bs) (c:cs) -}

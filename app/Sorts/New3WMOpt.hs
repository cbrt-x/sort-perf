module Sorts.New3WMOpt (sort, sortBy) where
import Data.List.NonEmpty (NonEmpty(..))
import Test.Tasty.QuickCheck (NonEmptyList(NonEmpty))
import Debug.Trace (traceShow, traceShowId)

sort :: (Show a, Ord a) => [a] -> [a]
sort = sortBy compare

-- sortBy :: (a -> a -> Ordering) -> [a] -> [a]
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

    {- merge' as@(a:as') bs@(b:bs') cs@(c:cs')
      | a_gt_b, b_gt_c = c : merge'gt as (b:|bs') cs'  -- a > b > c
      | a_gt_b         = b : merge'   as bs' cs  -- a > b <= c
      | a_gt_c         = c : merge'le (a:|as') bs cs'  -- c < a <= b
      | otherwise      = a : merge'   as' bs cs  -- c >= a <= b
      where a_gt_b = a `gt` b
            a_gt_c = a `gt` c
            b_gt_c = b `gt` c
    merge' [] bs cs = merge bs cs
    merge' as [] cs = merge as cs
    merge' as bs [] = merge as bs -}
    
    merge' as@(a:as') bs@(b:bs') cs@(c:cs')
      -- = let (as'', cs'') = min' (a:|as') (c:|cs')
      --      (af, bs'') = min' as'' (b:|bs')
      --      (bf, cf) = min' bs'' cs''
      = let (a1, b1) = min' (a:|as') (b:|bs')
            (b2, cf) = min' b1 (c:|cs')
            (af, bf) = min' a1 b2
        in merge's af bf cf
      -- where a_gt_b = a `gt` b
      --      a_gt_c = a `gt` c
      --      b_gt_c = b `gt` c
    merge' [] bs cs = merge bs cs
    merge' as [] cs = merge as cs
    merge' as bs [] = merge as bs

    min' as@(a:|_) bs@(b:|_)
      | a `gt` b  = (bs, as)
      | otherwise = (as, bs)


    merge's (a:|(a':as')) bs@(b:|_) cs@(c:|_) = a : foo (a':|as')
      where foo as | b `gt` a' = merge's as bs cs
                   | c `gt` a' = merge's bs as cs
                   | otherwise = merge's bs cs as
    merge's (a:|[]) (b:|bs) (c:|cs) = a : merge (b:bs) (c:cs)


    merge'gt as bs@(b:|bs') cs@(c:cs')
      | b_gt_c    = c : merge'gt as bs cs'  -- a > b > c
      | otherwise = b : merge'   as bs' cs  -- a > b <= c
      where b_gt_c = b `gt` c
    merge'gt as (b:|bs) [] = b : merge as bs

    merge'le as@(a:|as') bs cs@(c:cs')
      | a_gt_c         = c : merge'le as bs cs'  -- c < a <= b
      | otherwise      = a : merge'   as' bs cs  -- c >= a <= b
      where a_gt_c = a `gt` c
    merge'le (a:|as) bs [] = a : merge as bs

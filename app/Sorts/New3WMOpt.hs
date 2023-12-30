module Sorts.New3WMOpt (sort, sortBy) where
import Data.List.NonEmpty (NonEmpty(..))

sort :: Ord a => [a] -> [a]
sort = sortBy compare

sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy cmp = mergeAll . sequences
  where
    sequences (a:b:xs)
      | a `cmp` b == GT = descending b [a]  xs
      | otherwise       = ascending  b (a:) xs
    sequences xs = [xs]

    descending a as (b:bs)
      | a `cmp` b == GT = descending b (a:as) bs
    descending a as bs  = (a:as): sequences bs

    ascending a as (b:bs)
      | a `cmp` b /= GT = ascending b (\ys -> as (a:ys)) bs
    ascending a as bs   = let !x = as [a]
                          in x : sequences bs

    mergeAll [x] = x
    mergeAll xs  = mergeAll (mergePairs xs)

    -- optional case that'll speed up the last merge a tiny bit
    mergePairs [a,b,c]       = [merge' a b c]
    mergePairs (a:b:c:xs) = let !x = merge' a b c
                            in x : mergePairs xs
    mergePairs [a,b]      = [merge a b]
    mergePairs xs         = xs

    x `gt` y = x `cmp` y == GT

    merge as@(a:as') bs@(b:bs')
      | a `gt` b   = b : merge as  bs'
      | otherwise  = a : merge as' bs
    merge [] bs   = bs
    merge as []   = as

    merge' as@(a:as') bs@(b:bs') cs@(c:cs')
      | a_gt_b, b_gt_c = c : merge'gt as (b:|bs') cs'  -- a > b > c
      | a_gt_b         = b : merge'   as bs' cs  -- a > b <= c
      | a_gt_c         = c : merge'le (a:|as') bs cs'  -- c < a <= b
      | otherwise      = a : merge'   as' bs cs  -- c >= a <= b
      where a_gt_b = a `gt` b
            a_gt_c = a `gt` c
            b_gt_c = b `gt` c
    merge' [] bs cs = merge bs cs
    merge' as [] cs = merge as cs
    merge' as bs [] = merge as bs

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


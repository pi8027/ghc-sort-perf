module Sorts.NewStrict (sort, sortBy) where

{-# INLINEABLE sort #-}
sort :: Ord a => [a] -> [a]
sort = actualSort (>)

{-# INLINEABLE sortBy #-}
sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy cmp = actualSort (\x y -> cmp x y == GT)

{-# INLINE actualSort #-}
actualSort :: (a -> a -> Bool) -> [a] -> [a]
actualSort gt ns
  | []        <- ns = []
  | [a]       <- ns = [a]
  | otherwise       = merge_all (sequences ns)
  where
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

    merge_all [x] = x
    merge_all xs  = rev_merge_all (reduce xs)

    rev_merge_all [x] = reverse x
    rev_merge_all xs = merge_all (revreduce xs)

    reduce []            = []
    reduce [a]           = [reverse a]
    reduce [a,b]         = [merge a b []]
    reduce [a,b,c]       = [merge3 a b c []]
    reduce [a,b,c,d,e]   = [merge a b [], merge3 c d e []]
    reduce [a,b,c,d,e,f] = [merge3 a b c [], merge3 d e f []]
    reduce (a:b:c:d:xs)  = let !x = merge4 a b c d []
                           in x : reduce xs

    revreduce []            = []
    revreduce [a]           = [reverse a]
    revreduce [a,b]         = [revmerge b a []]
    revreduce [a,b,c]       = [revmerge3 c b a []]
    revreduce [a,b,c,d,e]   = [revmerge b a [], revmerge3 e d c []]
    revreduce [a,b,c,d,e,f] = [revmerge3 c b a [], revmerge3 f e d []]
    revreduce (a:b:c:d:xs)  = let !x = revmerge4 d c b a []
                              in x : revreduce xs

    revcat [] acc = acc
    revcat (a:as) acc = revcat as (a:acc)

    merge as@(a:as') bs@(b:bs') acc
      | a `gt` b  = merge as  bs' (b:acc)
      | otherwise = merge as' bs (a:acc)
    merge [] bs acc = revcat bs acc
    merge as [] acc = revcat as acc

    merge3 as@(a:as') bs@(b:bs') cs acc
      | a `gt` b  = merge3X b as  bs' cs acc
      | otherwise = merge3X a as' bs  cs acc
    merge3 [] bs cs acc = merge bs cs acc
    merge3 as [] cs acc = merge as cs acc

    merge3X x as bs cs@(c:cs') acc
      | x `gt` c  = merge3X x as bs cs' (c:acc)
      | otherwise = merge3    as bs cs (x:acc)
    merge3X x as bs [] acc = merge as bs (x:acc)

    merge3Y as@(a:as') y bs cs acc
      | a `gt` y  = merge3  as    bs cs (y:acc)
      | otherwise = merge3Y as' y bs cs (a:acc)
    merge3Y [] x bs cs acc = merge bs cs (x:acc)

    merge4 as@(a:as') bs@(b:bs') cs ds acc
      | a `gt` b  = merge4X b as  bs' cs ds acc
      | otherwise = merge4X a as' bs  cs ds acc
    merge4 [] bs cs ds acc = merge3 bs cs ds acc
    merge4 as [] cs ds acc = merge3 as cs ds acc

    merge4X x as bs cs@(c:cs') ds@(d:ds') acc
      | c `gt` d  = merge4XY x as bs d cs  ds' acc
      | otherwise = merge4XY x as bs c cs' ds acc
    merge4X x as bs [] ds acc = merge3X x as bs ds acc
    merge4X x as bs cs [] acc = merge3X x as bs cs acc

    merge4Y as@(a:as') bs@(b:bs') y cs ds acc
      | a `gt` b  = merge4XY b as  bs' y cs ds acc
      | otherwise = merge4XY a as' bs  y cs ds acc
    merge4Y as [] y cs ds acc = merge3Y as y cs ds acc
    merge4Y [] bs y cs ds acc = merge3Y bs y cs ds acc

    merge4XY x as bs y cs ds acc
      | x `gt` y  = merge4X x as bs   cs ds (y:acc)
      | otherwise = merge4Y   as bs y cs ds (x:acc)

    -- reversal versions
    revmerge as@(a:as') bs@(b:bs') acc
      | b `gt` a  = revmerge as  bs' (b:acc)
      | otherwise = revmerge as' bs (a:acc)
    revmerge [] bs acc = revcat bs acc
    revmerge as [] acc = revcat as acc

    revmerge3 as@(a:as') bs@(b:bs') cs acc
      | b `gt` a  = revmerge3X b as  bs' cs acc
      | otherwise = revmerge3X a as' bs  cs acc
    revmerge3 [] bs cs acc = revmerge bs cs acc
    revmerge3 as [] cs acc = revmerge as cs acc

    revmerge3X x as bs cs@(c:cs') acc
      | c `gt` x  = revmerge3X x as bs cs' (c:acc)
      | otherwise = revmerge3    as bs cs (x:acc)
    revmerge3X x as bs [] acc = revmerge as bs (x:acc)

    revmerge3Y as@(a:as') y bs cs acc
      | y `gt` a  = revmerge3  as    bs cs (y:acc)
      | otherwise = revmerge3Y as' y bs cs (a:acc)
    revmerge3Y [] x bs cs acc = revmerge bs cs (x:acc)

    revmerge4 as@(a:as') bs@(b:bs') cs ds acc
      | b `gt` a  = revmerge4X b as  bs' cs ds acc
      | otherwise = revmerge4X a as' bs  cs ds acc
    revmerge4 [] bs cs ds acc = revmerge3 bs cs ds acc
    revmerge4 as [] cs ds acc = revmerge3 as cs ds acc

    revmerge4X x as bs cs@(c:cs') ds@(d:ds') acc
      | d `gt` c  = revmerge4XY x as bs d cs  ds' acc
      | otherwise = revmerge4XY x as bs c cs' ds acc
    revmerge4X x as bs [] ds acc = revmerge3X x as bs ds acc
    revmerge4X x as bs cs [] acc = revmerge3X x as bs cs acc

    revmerge4Y as@(a:as') bs@(b:bs') y cs ds acc
      | b `gt` a  = revmerge4XY b as  bs' y cs ds acc
      | otherwise = revmerge4XY a as' bs  y cs ds acc
    revmerge4Y as [] y cs ds acc = revmerge3Y as y cs ds acc
    revmerge4Y [] bs y cs ds acc = revmerge3Y bs y cs ds acc

    revmerge4XY x as bs y cs ds acc
      | y `gt` x  = revmerge4X x as bs   cs ds (y:acc)
      | otherwise = revmerge4Y   as bs y cs ds (x:acc)

import Test.QuickCheck
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)

-- exercise 2.1
suffixes [] = []
suffixes xs = xs:suffixes (tail xs)

-- exercise 2.[2..5]
class Set t where
    emptyS :: t e
    insertS :: (Ord e) => e -> t e -> t e
    memberS :: (Ord e) => e -> t e -> Bool

data Tree e = E | T e (Tree e) (Tree e) deriving (Show, Eq)
val (T n _ _) = n
left (T _ l _) = l
right (T _ _ r) = r

insert v E = T v E E
insert v t@(T n left right)
  | v < n = T n (insert v left) right
  | n < v = T n left (insert v right) 
  | otherwise = t

member v E = False
member v (T n left right)
  | v < n = member v left
  | n < v = member v right
  | otherwise = True

instance Set Tree where
    emptyS = E
    insertS = insert
    memberS = member

-- exercise 2.2
member' v E = False
member' v t = m v t $ val t
    where m v E c = v == c
          m v (T n l r) c
            | v < n = m v l c
            | otherwise = m v r n

-- exercise 2.3
insert' v tree = fromMaybe tree (ins v tree)
    where ins v E = Just $ T v E E
          ins v (T n left right)
            | v < n = flip (T n) right <$> ins v left
            | n < v = T n left <$> ins v right
            | otherwise = Nothing 

-- exercise 2.4
insert'' v E = T v E E
insert'' v t = fromMaybe t (ins v t $ val t)
    where ins v E c
            | c == v = Nothing
            | otherwise = Just $ T v E E
          ins v (T n left right) c
            | v < n = flip (T n) right <$> ins v left c
            | otherwise = T n left <$> ins v right n

tr = foldr insert'' (emptyFM :: Tree e) [1,2..20]

-- exercise 2.5
-- (a)
complete n d
    | d == 0 = E
    | d == 1 = T n E E
    | otherwise = T n sub sub
    where sub = complete n $ pred d

-- (b)
complete' n m
  | m == 0 = E
  | m == 1 = T n E E
  | m `mod` 2 == 0 = T n full rest
  | otherwise = T n full full
    where
        sub = complete' n $ pred m
        (rest, full) = create2 n (m `div` 2)
        create2 n m = (complete' n $ pred m, complete' n m)

-- exercise 2.6
class FiniteMap t where
    emptyFM :: t kv
    bindFM :: (Ord k, Eq e) => k -> e -> t (k, e) -> t (k, e)
    lookupFM :: (Ord k, Eq e) => k -> t (k, e) -> Maybe e

instance FiniteMap Tree where
    emptyFM = E
    bindFM k v E = T (k, v) E E
    bindFM k v fm = fromMaybe fm (bind k v fm $ val fm)
        where bind k v E c = Just $ T (k, v) E E
              bind k v (T n left right) c
                | k < key = flip (T n) right <$> bind k v left c
                | key < k = T n left <$> bind k v right c
                | v /= val = Just $ T (k, v) left right
                | otherwise = Nothing
                where (key, val) = n

    lookupFM k E = Nothing
    lookupFM k fm = m k fm $ val fm
        where m k E c
                | k == fst c = Just $ snd c
                | otherwise = Nothing
              m k (T n l r) c
                | k < fst n = m k l c
                | otherwise = m k r n

fromList by = foldr by E
fromkvs ks vs = foldr ($) (emptyFM :: Tree e) $ zipWith ($) (map bindFM ks) vs

prop_2'2 x xs = let tree = fromList insert xs
                 in member x tree == member' x tree
prop_2'3 xs = fromList insert xs == fromList insert' xs
prop_2'4 xs = fromList insert xs == fromList insert'' xs
prop_2'6'1 vs = let fm = fromkvs vs vs
                  in map Just vs == map (`lookupFM` fm) vs
prop_2'6'2 x vs = let fm = fromkvs vs vs
                      idx = elemIndex x vs
                   in lookupFM x fm == ((vs !!) <$> idx)
                   && lookupFM x (bindFM x x fm) == Just x

main = do
    Test.QuickCheck.quickCheck (prop_2'2 :: Int -> [Int] -> Bool)
    Test.QuickCheck.quickCheck (prop_2'3 :: [Int] -> Bool)
    Test.QuickCheck.quickCheck (prop_2'4 :: [Int] -> Bool)
    Test.QuickCheck.quickCheck (prop_2'6'1 :: [Int] -> Bool)
    Test.QuickCheck.quickCheck (prop_2'6'2 :: Int -> [Int] -> Bool)

module HW1.T3
  ( Tree (..)
  , tsize
  , tdepth
  , tmember
  , tinsert
  , tFromList
  ) where

type Meta = (Int, Int)

data Tree a = Leaf | Branch Meta (Tree a) a (Tree a)
  deriving (Show)

mkBranch :: Tree a -> a -> Tree a -> Tree a
mkBranch l x r = Branch (tsize l + tsize r + 1, 1 + max (tdepth l) (tdepth r)) l x r

tsize :: Tree a -> Int
tsize Leaf = 0
tsize (Branch (x, _) _ _ _) = x

tdepth :: Tree a -> Int
tdepth Leaf = 0
tdepth (Branch (_, x) l _ r) = x

tDiff :: Tree a -> Int
tDiff Leaf = 0
tDiff (Branch _ l _ r) = tdepth l - tdepth r

tleft :: Tree a -> Tree a
tleft Leaf = Leaf
tleft (Branch _ l _ _) = l

tright :: Tree a -> Tree a
tright Leaf = Leaf
tright (Branch _ _ _ r) = r

tmember :: Ord a => a -> Tree a -> Bool
tmember _ Leaf = False
tmember x (Branch _ l y r)
  | x == y = True
  | x > y = tmember x r
  | otherwise = tmember x l

tinsert :: Ord a => a -> Tree a -> Tree a
tinsert x Leaf = mkBranch Leaf x Leaf
tinsert x (Branch m l y r)
  | x == y = (Branch m l y r)
  | x < y = balanceLeft $ mkBranch (tinsert x l) y r
  | otherwise = balanceRight $ mkBranch l y (tinsert x r)

balanceLeft :: Tree a -> Tree a
balanceLeft Leaf = Leaf
balanceLeft t@(Branch _ l x r) = if (tDiff t <= 1)
                                  then t
                                  else if (tDiff l >= 0)
                                       then smallRightRotate t
                                       else bigRightRotate t

balanceRight :: Tree a -> Tree a
balanceRight Leaf = Leaf
balanceRight t@(Branch _ l x r) = if (tDiff t >= -1)
                                  then t
                                  else if (tDiff r <= 0)
                                       then smallLeftRotate t
                                       else bigLeftRotate t

smallLeftRotate :: Tree a -> Tree a
smallLeftRotate (Branch _ l x (Branch _ rl y rr)) = mkBranch (mkBranch l x rl) y rr

smallRightRotate :: Tree a -> Tree a
smallRightRotate (Branch _ (Branch _ ll y lr) x r) = mkBranch ll y (mkBranch lr x r)

bigLeftRotate :: Tree a -> Tree a
bigLeftRotate (Branch _ l x r) = smallLeftRotate $ mkBranch l x (smallRightRotate r)

bigRightRotate :: Tree a -> Tree a
bigRightRotate (Branch _ l x r) = smallRightRotate $ mkBranch (smallLeftRotate l) x r

tFromList :: Ord a => [a] -> Tree a
tFromList a = foldr tinsert Leaf a


module TreeFolds where

import qualified Data.DList as DL
import Test.HUnit

-- | a basic tree data structure
data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

infixOrder :: Tree a -> [a]
infixOrder Empty = []
infixOrder (Branch x l r) = infixOrder l ++ [x] ++ infixOrder r

exTree :: Tree Int
exTree =
  Branch
    5
    (Branch 2 (Branch 1 Empty Empty) (Branch 4 Empty Empty))
    (Branch 9 Empty (Branch 7 Empty Empty))

testInfixOrder :: Test
testInfixOrder = "infixOrder" ~: infixOrder exTree ~?= [1, 2, 4, 5, 9, 7]

-- | A big "right-skewed" tree
bigRightTree :: Int -> Tree Int
bigRightTree m = go 0
  where
    go n = if n <= m then Branch n (go (n + 1)) Empty else Empty

-- | A big "left-skewed" tree
bigLeftTree :: Int -> Tree Int
bigLeftTree m = go 0
  where
    go n = if n <= m then Branch n Empty (go (n + 1)) else Empty

infixOrder1 :: Tree a -> [a]
infixOrder1 = undefined

tinfixOrder1 :: Test
tinfixOrder1 = "infixOrder1a" ~: infixOrder1 exTree ~?= [1, 2, 4, 5, 9, 7]

infixOrder2 :: Tree Int -> [Int]
infixOrder2 = undefined

infixOrder3 :: Tree Int -> [Int]
infixOrder3 = undefined

foldrTree :: (a -> b -> b) -> b -> Tree a -> b
foldrTree f b t = undefined

infixOrder4 :: Tree a -> [a]
infixOrder4 = foldrTree (:) []

sizeTree :: Tree Int -> Int
sizeTree = foldrTree (const (1 +)) 0

sumTree :: Tree Int -> Int
sumTree = foldrTree (+) 0

anyTree :: (a -> Bool) -> Tree a -> Bool
anyTree f = foldrTree (\x b -> f x || b) False

allTree :: (a -> Bool) -> Tree a -> Bool
allTree f = foldrTree (\x b -> f x && b) True

foldlTree :: (b -> a -> b) -> b -> Tree a -> b
foldlTree = undefined

revOrder :: Tree a -> [a]
revOrder = foldlTree (flip (:)) []

trevOrder :: Test
trevOrder = "revOrder" ~: revOrder exTree ~?= [7, 9, 5, 4, 2, 1]

foldTree :: (a -> b -> b -> b) -> b -> Tree a -> b
foldTree _ e Empty = e
foldTree f e (Branch a n1 n2) = f a (foldTree f e n1) (foldTree f e n2)

foldrTree' :: (a -> b -> b) -> b -> Tree a -> b
foldrTree' = undefined

tree1 :: Tree Int
tree1 = Branch 1 (Branch 2 Empty Empty) (Branch 3 Empty Empty)

tfoldrTree' :: Test
tfoldrTree' = "foldrTree'" ~: foldrTree' (+) 0 tree1 ~?= 6

foldlTree' :: (b -> a -> b) -> b -> Tree a -> b
foldlTree' = undefined

tfoldlTree' :: Test
tfoldlTree' = "foldlTree'" ~: foldlTree' (+) 0 tree1 ~?= 6

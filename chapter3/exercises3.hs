import Data.List

-- 1. Write a function that computes the length of a list
-- 2. Add a type signature for your function. 
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

-- 3. Write a function that computes the mean of a list (i.e sum/length)
mean :: [Double] -> Double
mean list = sum list / fromIntegral (myLength list)

-- 4. Write a function that takes a list and makes it into a palindrome (e.x [1,2,3] -> [1,2,3,3,2,1])
palindrome :: [a] -> [a]
palindrome list = list ++ reverse list

-- 5. Write a function that checks if a list is a palindrome
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome list = list == reverse list

-- 6. Write a function that sorts a list of lists based on the length of each sublist
sortLists :: [[a]] -> [[a]]
sortLists lists = sortBy(\a b -> compare (length a) (length b)) lists

-- 7. Define a function that joins a list together with a seperator value
myIntersperse :: a -> [[a]] -> [a]
myIntersperse _ [] = []
myIntersperse _ [a] = a
myIntersperse delim (a:ls) = a ++ delim:(myIntersperse delim ls)

-- 8. Using the data type defined earlier (pg. 58), write a function that calculates the height of a Tree.
-- Empty has height 0, Node "x" Empty Empty has height 1, Node "x" Empty (Node "y" Empty Empty) has height two and so on

-- Tree data type from Chapter 3 page 58
data Tree a = Node a (Tree a) (Tree a)
            | Empty
            deriving(Show)

smolTree = Node 10 (Node 5 (Node 4 Empty Empty) (Node 8 (Node 7 Empty Empty) Empty))
                   (Node 12 Empty (Node 20 (Node 15 Empty Empty) Empty))

-- Simple tree of height 2
simpleTree = Node "parent" (Node "left child" Empty Empty)
                           (Node "right child" Empty Empty)

-- More complex tree structure, should have height 5. tree visualization:
{-         a
         /  \
        b    c
      /  \    \
     d    e    i
    /    /
   f    h
    \
    g
-}
complexTree = Node "a" (Node "b" (Node "d" (Node "f" Empty (Node "g" Empty Empty)) Empty) (Node "e" (Node "h" Empty Empty) Empty))
                       (Node "c" Empty (Node "i" (Node "j" Empty Empty) Empty ))

treeHeight :: Tree a -> Int
treeHeight Empty = 0
treeHeight (Node _ l r) = 1 + maximum [treeHeight l, treeHeight r]

-- 9. Assume three 2D points a,b and c that are connected in a line from a to b and
-- from b to c. The two lines can turn in a direction that's either Left Right
-- or Straight. Define a data type to represent those three occurences
data Direction = Left
               | Right
               | Straight
               deriving(Eq, Show)

-- 10. Write a function that calculates the turn made by three 2D points and
-- returns the direction.
type Point = (Double, Double)
findTurn :: Point -> Point -> Point -> Direction
findTurn a b c | zxprod > 0 = Main.Left
               | zxprod < 0 = Main.Right
               | otherwise  = Main.Straight
    where zxprod = (fst b - fst a)*(snd c - snd a) - (snd b - snd a)*(fst c - fst a)

-- 11. Write a function that takes a list of 2D points and computes the direction
-- for each successive triple of points and returns a list of directions.
-- For example the list of points [a,b,c,d,e] should calculate the directions of
-- [a,b,c], [b,c,d], [c,d,e] and return a list [Direction]

getDirections :: [Point] -> [Direction]
getDirections (a:b:c:ps) = (findTurn a b c) : getDirections(b:c:ps)
getDirections _ = []

-- 12. Using the preceding code, Implement graham's scan algorithm for the
-- convex hull of a set of 2D points. Follow the links below for descriptions
-- about the problem
-- https://en.wikipedia.org/wiki/Convex_hull
-- https://en.wikipedia.org/wiki/Graham_scan

pivotPoint :: [Point] -> Point
pivotPoint = minimumBy (\a b -> comparePoints a b)
           where comparePoints (x0,y0) (x1,y1) | y0 == y1  = compare x0 x1
                                               | otherwise = compare y0 y1

sortPoints :: [Point] -> [Point]
sortPoints ps = sortBy (\a b -> compare (theta a) (theta b)) ps
           where theta (x,y) = atan2 (y - snd p0) (x - fst p0)
                 p0 = pivotPoint ps

grahamScan :: [Point] -> [Point] -> [Point]
grahamScan (s2:s1:s) (pi:ps) | findTurn s1 s2 pi == Main.Right = grahamScan (s1:s) (pi:ps)
                             | otherwise = grahamScan (pi:s2:s1:s) ps
grahamScan stack [] = stack

convexHull :: [Point] -> [Point]
convexHull = let ch (a:b:c:ps) = grahamScan [c,b,a] ps
                 ch _ = []
             in reverse . ch . sortPoints

-- Sample points for testing out exercises 10-12
points :: [Point]
points = [(0.0,0.0),(1.2,3.6),(-2.2,2.9),(3.3,4.7),(-4.0,8.0),(8.0,12.0),(-7.0,7.0),(4.1,-3.0),(2.1,-15.3),(-18.0, 4.7),(-18.0, 3.3),(2.0,2.8),(-5.2,-7.9),(3.1,-15.3),(0.1,0.9),(-9.5,4.5)]

lotsOfPoints :: [Point]
lotsOfPoints = [(-9.6, 0.5), (11.5, -6.5), (2.9, -3.1), (14.8, 1.2), (1.4, 14.2), (9.4, -15.5), (-1.6, 18.7), (-6.5, -12.3), (4.1, 3.0), (4.4, 7.9), (-10.2, -13.1), (-3.3, 12.3), (5.0, -15.4), (4.5, 7.8), (1.1, -1.7), (8.5, 6.6), (15.3, -17.1), (19.2, -12.7), (-9.3, 8.0), (-16.0, 12.2), (-4.5, 11.5), (-5.8, -18.7), (0.8, -14.4), (12.9, -15.4), (-6.4, -14.5), (-15.7, -18.2), (17.9, -10.6), (18.6, 18.1), (2.0, -9.2), (10.0, 7.0), (11.1, -0.7), (-18.6, 14.7), (-1.2, -9.3), (-15.0, -7.8), (-12.5, -16.2), (-15.1, -5.1), (-19.0, 18.8), (0.1, -14.9), (16.2, -9.2), (14.6, 13.7), (-7.3, -8.4), (5.3, -17.2), (-5.4, 8.6), (10.0, 3.6), (6.1, 14.4), (17.1, 3.1), (16.7, -18.5), (16.1, 9.7), (-10.0, 4.6), (13.3, -12.9), (-0.4, -9.8), (-14.2, 14.4), (10.9, -10.4), (14.2, -14.2), (-9.7, 11.0), (-1.3, -3.2), (5.2, -7.2), (13.1, 11.5), (7.0, -18.2), (8.3, -10.9), (-0.9, -1.8), (14.7, -17.9), (9.5, -8.3), (-13.9, 7.3), (8.3, 2.0), (5.3, 9.3), (3.4, -6.8), (-19.2, -15.6), (19.8, -16.6), (2.4, 18.8), (3.2, 17.6), (14.4, -19.3), (6.8, 18.4), (-5.2, 9.6), (-1.1, 18.0), (14.9, 18.3), (13.5, -17.4), (-0.8, 15.9), (-15.0, 5.1), (-16.7, 0.9), (-8.9, -4.3), (-4.3, -7.2), (-11.9, -19.9), (15.7, 2.5), (19.2, 11.2), (10.7, -1.1), (-9.0, 11.8), (-17.9, 13.0), (19.0, -2.1), (-5.8, -15.3), (11.7, -0.5), (-3.0, 18.6), (-2.5, -8.7), (13.8, 2.3), (-6.9, -13.2), (17.0, -3.2), (7.8, 10.7), (9.9, 17.1), (10.1, 17.0), (-10.0, 5.8)]
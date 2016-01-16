module Main where

import Data.List (find, intercalate, nub, permutations, sort)

pairs :: [a] -> [(a,a)]
pairs [] = []
pairs [x] = []
pairs (x1:x2:xs) = (x1,x2) : pairs xs

exponentialSum :: Floating a => [(a,a)] -> a
exponentialSum = foldl (+) 0 . map (\(b,e) -> b ** e)

showExponentialSum :: (RealFrac a, Show a) => [(a,a)] -> String
showExponentialSum = intercalate " + " . map (\(b,e) -> (show $ round b) ++ "^" ++ (show $ round e))

findAllPandigitalSums :: (Floating a, Ord a) => a -> [[(a, a)]]
findAllPandigitalSums n = nub $ filter (\ds -> exponentialSum ds == n) $ map (sort . pairs) $ permutations [0,1,2,3,4,5,6,7,8,9]

main = do
  mapM (putStrLn . showExponentialSum) $ findAllPandigitalSums 2016


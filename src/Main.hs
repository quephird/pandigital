module Main where

import Data.List (find, intercalate, nub, permutations, sort)

pairs :: [a] -> [(a,a)]
pairs [] = []
pairs [x] = []
pairs (x1:x2:xs) = (x1,x2) : pairs xs

data ExponentialSum = ExponentialSum [(Int, Int)]

instance Show ExponentialSum where
  show (ExponentialSum ds) = intercalate " + " $ map (\(b,e) -> (show b) ++ (lookup e)) ds where
    lookup e = ["⁰¹²³⁴⁵⁶⁷⁸⁹" !! e]

exponentialSum :: [(Int, Int)] -> Int
exponentialSum = foldl (+) 0 . map (\(b,e) -> b ^ e)

findAllPandigitalSums :: Int -> [ExponentialSum]
findAllPandigitalSums n = map ExponentialSum $ nub $ filter (\ds -> exponentialSum ds == n) $ map (sort . pairs) $ permutations [0,1,2,3,4,5,6,7,8,9]

main = do
  putStrLn "Enter a year:"
  input <- getLine
  let year = read input :: Int in
    mapM (putStrLn . show) $ findAllPandigitalSums year

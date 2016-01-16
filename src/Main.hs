module Main where

import Data.List (find, intercalate, permutations)

splitByPairs :: [a] -> ([a], [a])
splitByPairs = foldr (\e (xs, ys) -> (e : ys, xs)) ([], [])

exponentialSum :: Floating a => [a] -> a
exponentialSum = foldl (+) 0 . (uncurry $ zipWith (**)) . splitByPairs

findPandigitalSum :: (Eq a, Floating a) => a -> Maybe [a]
findPandigitalSum n = find (\ds -> exponentialSum ds == n) $ permutations [0..9]

printExponentialSum :: (RealFrac a, Show a) => Maybe [a] -> IO()
printExponentialSum Nothing =
  putStrLn "No solution found!!!"
printExponentialSum (Just ds) = do
  putStrLn $ intercalate " + " $ makePairs ds where
    makePairs = map (\(b,e) -> show b ++ "^" ++ show e) . (uncurry $ zipWith (,)) . splitByPairs . map round

main = do
  mapM printExponentialSum $ map findPandigitalSum [2016..2050]

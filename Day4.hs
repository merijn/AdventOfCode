{-# LANGUAGE TupleSections #-}
import Data.List (scanl')
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IM
import System.Environment (getArgs)
import System.Exit (exitFailure)

toDigit :: Char -> Maybe Int
toDigit c | c >= '0' && c <= '9' = Just $ read [c]
toDigit _ = Nothing

simplifyLower :: [Int] -> [Int]
simplifyLower [] = []
simplifyLower (i:is) = map fst $ scanl' go (i, False) is
  where
    go :: (Int, Bool) -> Int -> (Int, Bool)
    go (x, False) n | x <= n = (n, False)
    go (x, _) _ = (x, True)

digitCounts :: [Int] -> IntMap Int
digitCounts = IM.fromListWith (+) . map (,1)

puzzle1Criteria :: [Int] -> Bool
puzzle1Criteria = any (>=2) . IM.elems . digitCounts

puzzle2Criteria :: [Int] -> Bool
puzzle2Criteria = any (==2) . IM.elems . digitCounts

generate :: ([Int] -> Bool) -> [Int] -> [Int] -> [[Int]]
generate f lowerbound upperbound = filter f $ go lowerbound upperbound
  where
    go :: [Int] -> [Int] -> [[Int]]
    go [] [] = [[]]
    go (lb:lbs) (ub:ubs)  = do
        n <- [lb .. ub]

        let updateLower | lb == n = map (max n)
                        | otherwise = map (const n)

            updateUpper | ub == n = id
                        | otherwise = map (const 9)

        rest <- go (updateLower lbs) (updateUpper ubs)
        return (n:rest)

    go _ _ = []

main :: IO ()
main = do
    args <- getArgs
    (lower, upper)<- case args of
        [lower, upper]
            | Just result <- (,) <$> mapM toDigit lower <*> mapM toDigit upper
            -> return result
        _ -> putStrLn "Expected two numbers!" >> exitFailure

    let simpleLower = simplifyLower lower

    putStrLn $ "Lower bound: " ++ concatMap show lower
    putStrLn $ "Simplified lower bound: " ++ concatMap show simpleLower
    putStrLn $ "Upper bound: " ++ concatMap show upper
    print (length $ generate puzzle1Criteria simpleLower upper)
    print (length $ generate puzzle2Criteria simpleLower upper)

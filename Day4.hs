import Data.List (scanl')
import Data.Either (rights)
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

generate :: [Int] -> [Int] -> [[Int]]
generate lowerbound upperbound = rights $ go lowerbound upperbound
  where
    go :: [Int] -> [Int] -> [Either [Int] [Int]]
    go [] [] = [Left []]
    go (lb:lbs) (ub:ubs)  = do
        n <- [lb .. ub]

        let updateLower | lb == n = map (max n)
                        | otherwise = map (const n)

            updateUpper | ub == n = id
                        | otherwise = map (const 9)

        rest <- go (updateLower lbs) (updateUpper ubs)
        return $ case rest of
            Left l@(h:_) | h == n -> Right (n:l)
            Left l -> Left (n:l)
            Right l -> Right (n:l)

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
    print (length $ generate simpleLower upper)

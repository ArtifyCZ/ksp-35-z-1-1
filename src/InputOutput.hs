module InputOutput
    ( readInput
    , writeOutput
    ) where

import Control.Monad (replicateM)
import Data.List (sort)

readFirstRow :: IO (Int, Int)
readFirstRow = do
    line <- getLine
    let input = words line
    let numbers = map read input
    let n = head numbers
    let q = head $ tail numbers
    return (n, q)

readDictionary :: Int -> IO [String]
readDictionary n = fmap sort $ readDict
    where
    readDict = replicateM n getLine

readQuestions :: Int -> IO [(String, String)]
readQuestions n = replicateM n readQ
    where
    readQ = do
        line <- fmap words getLine
        let a = head line
        let b = head $ tail line
        return (a, b)

readInput :: IO ([String], [(String, String)])
readInput = do
    (n, q) <- readFirstRow
    dictionary <- readDictionary n
    questions <- readQuestions q
    return (dictionary, questions)

writeOutput :: [Int] -> IO ()
writeOutput answers = mapM_ print answers

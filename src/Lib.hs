module Lib
    ( app
    ) where

import qualified Data.Map as Map
import InputOutput (readInput, writeOutput)

type Dict = Map.Map String Int

answer :: Dict -> (String, String) -> Int
answer dict (a, b) = abs (ai - bi) - 1
    where
    ai = getFromDict dict a
    bi = getFromDict dict b


makeDict :: [String] -> Dict
makeDict words = Map.fromList $ zip words [0..(length words)]

getFromDict :: Dict -> String -> Int
getFromDict dict x = unwrap $ Map.lookup x dict
    where
    unwrap (Just i) = i

app :: IO ()
app = do
    (words, questions) <- readInput
    let dict = makeDict words
    let answers = map (\q -> answer dict q) questions
    writeOutput answers

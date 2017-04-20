module Main where

import Data.Map
import Data.Set hiding (null, foldr, foldl)


import System.Environment
import System.Exit
import System.IO

import Eval
import Parse

processArgs :: [String] -> Map String String
processArgs [] = Data.Map.empty
processArgs ("-u":ss) = Data.Map.insert "Unsafe" "" (processArgs ss)
processArgs ("-":ss) = processArgs ss
processArgs (s:ss) = Data.Map.insert "Filename" s (processArgs ss)

main :: IO ()
main = do
  args <- getArgs
  let pArgs = processArgs args
  input <- case Data.Map.lookup "Filename" pArgs of
    Nothing -> getContents
    Just fn -> readFile fn
  let parsed = case pLExp input of
        (Left e) -> e
        (Right exp) -> exp
  let evaluated = eval parsed
  case evaluated of
    (Left e) -> e
    (Right exp) -> putStr $ show exp

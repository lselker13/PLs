module Main where

import Data.Map
import Data.Set hiding (null, foldr, foldl)


import System.Environment
import System.Exit
import System.IO

import Eval
import Parse
import TypeChecker

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
  let evaluated = if Data.Map.member "Unsafe" pArgs
        then case typeOf Data.Map.empty parsed of
               Right _ -> eval parsed
               Left e -> error $ show e
        else eval parsed
  case evaluated of
    (Left e) -> error $ show e
    (Right exp) -> putStr $ show exp

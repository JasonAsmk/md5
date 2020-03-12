module Main where

import           MD5.Lib
import           System.Environment

main :: IO ()
main = do
  args <- getArgs
  hash <- md5 $ head args
  putStrLn hash


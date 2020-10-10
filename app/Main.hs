module Main where

import Language.PureScript.Web

import qualified Data.Text.IO as T

-- TODO: everything
main :: IO ()
main = do
  (ws, out) <- compileTextToJS "module Main where\nmain = let f x = f x in f 1"
  printErrors ws
  either printErrors T.putStrLn out

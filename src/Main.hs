module Main where

import Syntax
import Parser
import Infer
import Eval

import Data.List (isPrefixOf)
import System.IO (hFlush, stdout)

main :: IO ()
main = do
  putStrLn $ "Fino 0.2.1.0"
  repl
  return ()

repl :: IO ()
repl = do
  putStr ">>> "
  hFlush stdout
  input <- getLine
  case input of
    s | s == ":quit" -> return ()
      | s == ":help" -> do { help; repl}
      | isPrefixOf ":load" s -> do
               let path = head $ tail $ words s
               putStrLn path
               contents <- readFile path
               run contents
               repl
      | null s       -> do { repl }
      | otherwise    -> do { run s; repl }

run :: String -> IO ()
run s = do
  let e = parseExpr s
  putStrLn $ "expr:  " ++ show e
  let t = inferExpr e
  putStrLn $ "type:  " ++ show t
  let v = eval e []
  putStrLn $ "value: " ++ show v

help :: IO ()
help = putStrLn "Type `:help` for help, `:exit` to exit" 

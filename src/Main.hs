module Main where

import Text.Parsec (parse)
import System.Console.ANSI

import Parser
import Reconstruction
import Syntax
import Pretty

main :: IO ()
main = do code <- getContents
          case parse parseProgram "code" code of
            Left  err -> do setSGR [SetColor Foreground Vivid Red]
                            putStrLn "Parse error"
                            setSGR [Reset]
                            print err
            Right ast -> do let r = reconstructProgram ast
                            mapM_ printDecl r

nUMBER_PHASES :: Int
nUMBER_PHASES = 2

printDecl :: [Decl] -> IO ()
printDecl = printDecl' 0

printDecl' :: Int -> [Decl] -> IO ()
printDecl' _ [] = return ()
printDecl' n (d:ds)
  | n > nUMBER_PHASES = putStrLn ""
  | otherwise = do setSGR [SetColor Foreground Vivid Blue]
                   putStrLn ("Phase " ++ show n)
                   setSGR [Reset]
                   putStrLn (pretty d)
                   printDecl' (n+1) ds

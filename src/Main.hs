module Main where

import Control.Monad (forM_)
import Text.Parsec (parse)
import System.Console.ANSI

import Parser
import Reconstruction
import Syntax
import Solver

main :: IO ()
main = do code <- getContents
          case parse parseProgram "code" code of
            Left  err -> do setSGR [SetColor Foreground Vivid Red]
                            putStrLn "Parse error:"
                            setSGR [Reset]
                            print err
            Right ast -> do setSGR [SetColor Foreground Vivid Blue]
                            putStrLn "Reconstruction"
                            putStrLn "--------------"
                            setSGR [Reset]
                            let r = reconstructProgram ast
                            forM_ r $ \(decl, cs) -> do
                              putStrLn ""
                              putStrLn (prettyDecl decl)
                              forM_ cs $ \(a, b) -> do
                                putStr "* "
                                putStr (prettyTy a)
                                putStr " ~ "
                                putStr (prettyTy b)
                                putStrLn ""
                              let decl' = mapExpr (\e -> solve' e cs) decl
                              putStrLn (prettyDecl decl')
                            putStrLn ""

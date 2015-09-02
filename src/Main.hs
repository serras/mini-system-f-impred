module Main where

import Control.Monad (forM_)
import Text.Parsec (parse)
import System.Console.ANSI

import Parser
import Reconstruction
import Solver
import Pretty

main :: IO ()
main = do code <- getContents
          case parse parseProgram "code" code of
            Left  err -> do setSGR [SetColor Foreground Vivid Red]
                            putStrLn "Parse error"
                            setSGR [Reset]
                            print err
            Right ast -> do let r = reconstructProgram ast
                            forM_ r $ \(decl, cs) -> do
                              putStrLn ""
                              setSGR [SetColor Foreground Vivid Blue]
                              putStrLn "Reconstructed"
                              setSGR [Reset]
                              putStrLn (pretty decl)
                              setSGR [SetColor Foreground Vivid Yellow]
                              putStrLn "Constraints"
                              setSGR [Reset]
                              forM_ cs $ \(a, b) -> do
                                putStr "* "
                                putStr (pretty a)
                                putStr " ~ "
                                putStr (pretty b)
                                putStrLn ""
                              let decl' = solve' decl cs
                              setSGR [SetColor Foreground Vivid Green]
                              putStrLn "Solved"
                              setSGR [Reset]
                              putStrLn (pretty decl')
                            putStrLn ""

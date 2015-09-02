module Main where

import Control.Monad (forM, forM_)
import Text.Parsec (parse)
import System.Console.ANSI

import Parser
import Reconstruction
import Solver
import Syntax
import Pretty

main :: IO ()
main = do code <- getContents
          case parse parseProgram "code" code of
            Left  err -> do setSGR [SetColor Foreground Vivid Red]
                            putStrLn "Parse error"
                            setSGR [Reset]
                            print err
            Right ast -> do let (i, r) = reconstructProgram ast
                            setSGR [SetColor Foreground Vivid Red]
                            putStrLn "PHASE 1"
                            putStrLn "======="
                            setSGR [Reset]
                            ast2 <- solveAndPrint r
                            let (_, r2) = reconstructProgram (i ++ ast2)
                            putStrLn ""
                            putStrLn ""
                            setSGR [SetColor Foreground Vivid Red]
                            putStrLn "PHASE 2"
                            putStrLn "======="
                            setSGR [Reset]
                            _ <- solveAndPrint r2
                            return ()

solveAndPrint :: [(Decl, ConstraintSet)] -> IO Program
solveAndPrint r = forM r $ \(decl, cs) -> do
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
  putStrLn "Solution"
  setSGR [Reset]
  putStrLn (pretty decl')
  return decl'

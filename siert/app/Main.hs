module Main where

import Programs
import ConcreteInterpreter
import TypeChecker

main :: IO ()
main = do
    let program = branchIf
    putStrLn $ check program
    putStrLn $ show (run program)

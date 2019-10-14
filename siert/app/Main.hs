module Main where

import Programs
import ConcreteInterpreter
import TypeChecker

main :: IO ()
main = do
    let program = branch1
    putStrLn $ check program
    putStrLn $ show (run program)

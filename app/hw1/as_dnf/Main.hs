module Main where

import Data.Map
import Data.Set
import Expr
import Lib

main :: IO ()
main = do
    s <- getLine
    let expr = read s :: Expr
    let nnf = toNNF expr
    let dnf = toDNF expr
    let cnf = toCNF1 expr
    -- putStrLn $ "original: " ++ (show expr)
    putStrLn $ "NNF: " ++ (show nnf)
    putStrLn $ "DNF: " ++ (show dnf)
    putStrLn $ "CNF: " ++ (show cnf)


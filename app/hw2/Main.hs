module Main where

import Data.Map
import Data.Set
import Data.List
import Data.Tuple
import Control.Monad

import Expr
import Lib
import System.Process
import GHC.IO.Handle

getVars :: Expr -> [Symb]
getVars (e1 :| e2) = getVars e1 ++ getVars e2
getVars (e1 :& e2) = getVars e1 ++ getVars e2
getVars (e1 :=> e2) = getVars e1 ++ getVars e2
getVars (e1 :<=> e2) = getVars e1 ++ getVars e2
getVars (Not e) = getVars e
getVars (Var s) = [s]

enumerate l = zip [1..] l

countOfClauses (e1 :& e2) = countOfClauses e1 + countOfClauses e2
countOfClauses (e1 :| e2) = 1
countOfClauses (Not e) = 1
countOfClauses (Var x) = 1
countOfClauses _ = undefined

cnfToMinisat mp_vars (e1 :& e2) = cnfToMinisat mp_vars e1 ++ "0\n" ++ cnfToMinisat mp_vars e2
cnfToMinisat mp_vars e@(e1 :| e2) = clauseToMinisat mp_vars e
cnfToMinisat mp_vars e@(Not (Var x)) = clauseToMinisat mp_vars e
cnfToMinisat mp_vars e@(Var x) = clauseToMinisat mp_vars e
cnfToMinisat _ _ = undefined
clauseToMinisat mp_vars (e1 :| e2) = clauseToMinisat mp_vars e1 ++ clauseToMinisat mp_vars e2
clauseToMinisat mp_vars (Not (Var x)) = "-" ++ clauseToMinisat mp_vars (Var x)
clauseToMinisat mp_vars (Var x) = show (mp_vars ! x) ++ " "
clauseToMinisat _ _ = undefined

mapVars expr = Data.Map.fromList $ Prelude.map swap $ enumerate $ Data.Set.toList $ Data.Set.fromList $ getVars expr

var r c = Var $ "x_" ++ show r ++ "_" ++ show c

concatAnd = foldl1 (:&)

queenExpr :: Int -> Expr
queenExpr n = everyRowHasQueen n :& oneQueenPerRow n :& oneQueenPerCol n :& oneQueenPerD1 n :& oneQueenPerD2 n
    where everyRowHasQueen n = foldl1 (:&) $ Prelude.map (
                \r -> foldr1 (:|) $ Prelude.map (
                    \c -> var r c
                    ) [1..n]) [1..n]
          oneQueenPerRow n = concatAnd $ Prelude.map (
              \r -> concatAnd $ Prelude.map (
                  \c1 -> concatAnd $ Prelude.map (
                      \c2 -> Not (var r c1) :| Not (var r c2) 
                      ) $ Prelude.filter (/= c1) [1..n]) [1..n]) [1..n]
          oneQueenPerCol n = foldl1 (:&) $ Prelude.map (
              \c -> foldl1 (:&) $ Prelude.map (
                  \r1 -> foldl1 (:&) $ Prelude.map (
                      \r2 -> Not (var r1 c) :| Not (var r2 c) 
                      ) $ Prelude.filter (/= r1) [1..n]) [1..n]) [1..n]
          oneQueenPerD1 n = foldl1 (:&) $ Prelude.map (
              \d -> let rs = Prelude.filter (<=n) $ Prelude.filter (>=1) [(1 - d)..(n - d)] in foldl1 (:&) $ Prelude.map (
                  \r1 -> foldl1 (:&) $ Prelude.map (
                      \r2 -> Not (var r1 (d+r1)) :| Not (var r2 (d+r2)) 
                      ) $ Prelude.filter (/= r1) rs) rs) [(2-n)..(n-2)]
          oneQueenPerD2 n = foldl1 (:&) $ Prelude.map (
              \d -> let rs = Prelude.filter (<=n) $ Prelude.filter (>=1) [(d - n)..(d - 1)] in foldl1 (:&) $ Prelude.map (
                  \r1 -> foldl1 (:&) $ Prelude.map (
                      \r2 -> Not (var r1 (d-r1)) :| Not (var r2 (d-r2)) 
                      ) $ Prelude.filter (/= r1) rs) rs) [3..(n+n-1)]
                


exprToMinisat :: Expr -> (String, Map String Int)
exprToMinisat expr = do
    let cnf = expr
    let mp_vars = mapVars cnf
    let cc = countOfClauses cnf
    let header = "p cnf " ++ show cc ++ " " ++ (show $ Data.Map.size mp_vars) ++ "\n"
    (header ++ (cnfToMinisat mp_vars cnf) ++ "0", mp_vars)

runMinisat text_cnf = do
    let infile= "/tmp/in.cnf"
    writeFile infile text_cnf

    let outfile= "/tmp/out.cnf"
    (_, Just hout, _, ph) <-
        createProcess (shell $ "minisat " ++ infile ++ " " ++ outfile){ std_out = CreatePipe }

    waitForProcess ph

    out <- readFile outfile
    return out

parseResult s mp_vars = do
    let mp = Data.Map.fromList $ Prelude.map Data.Tuple.swap $ Data.Map.toList mp_vars
    let (sat:ans) = words s
    if sat == "UNSAT" then
        undefined
    else
        do 
            let xs = Prelude.map (\i -> mp ! i) $ Prelude.filter (>0) $ Prelude.map (read) ans
            x <- xs
            guard (Data.List.take 2 x == "x_")
            (r, _c) <- lex $ Data.List.drop 2 x
            guard (Data.List.take 1 _c == "_")
            let c = Data.List.drop 1 _c
            return (r, c) --(read r :: Int, read c :: Int)       


main :: IO ()
main = do
    s <- getLine
    let n = read s :: Int

    let e = queenExpr n
    let (text_cnf, mp_vars) = exprToMinisat e

    text_sat <- runMinisat text_cnf
    let ans = parseResult text_sat mp_vars
   
    putStrLn $ show ans


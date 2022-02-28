-- {-# OPTIONS_GHC -F -pgmF hspec-discover #-}

import Test.HUnit
import Expr
import Lib

import TestExpr

-- tests = [ (Var "x", PrettyVar "x"), (Var "x" :& Var "x", PrettyAnd [PrettyVar "x", PrettyVar "x"]) ] :: [(Expr, PrettyExpr)]
        


main :: IO ()
main = do
    runTestTTAndExit $ testExpr
    
    




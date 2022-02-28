module TestExpr where

import Test.HUnit
import Expr

assertEqualList error_message f tests = do
    let (expected, input) = unzip tests
    let actual = map f input
    assertEqual error_message expected actual


testToPretty = TestCase $ do
    let error_message = "toPretty unit test failed!" 
    let tests = [
            (PrettyVar "x",
            Var "x"),

            (PrettyNot $ PrettyVar "x", 
            Not $ Var "x"),

            (PrettyAnd $ [PrettyVar "x_1", PrettyVar "x_2"],                    
            Var "x_1" :& Var "x_2"),

            (PrettyOr $ [PrettyVar "x_1", PrettyVar "x_2"], 
            Var "x_1" :| Var "x_2"),

            (PrettyImpl $ [PrettyVar "x_1", PrettyVar "x_2"], 
            Var "x_1" :=> Var "x_2"),

            (PrettyEq $ [PrettyVar "x_1", PrettyVar "x_2"], 
            Var "x_1" :<=> Var "x_2"),

            (PrettyAnd $ [PrettyVar "x_1", PrettyVar "x_2", PrettyVar "x_3"],   
            Var "x_1" :& Var "x_2" :& Var "x_3"),

            (PrettyOr $ [PrettyVar "x_1", PrettyVar "x_2", PrettyVar "x_3"],   
            Var "x_1" :| Var "x_2" :| Var "x_3"),

            (PrettyImpl $ [PrettyVar "x_1", PrettyVar "x_2", PrettyVar "x_3"],   
            Var "x_1" :=> Var "x_2" :=> Var "x_3"),

            (PrettyEq $ [PrettyVar "x_1", PrettyVar "x_2", PrettyVar "x_3"],   
            Var "x_1" :<=> Var "x_2" :<=> Var "x_3"),

            (PrettyOr $ [PrettyAnd $ [PrettyVar "x_1", PrettyVar "x_2"], PrettyVar "x_3"],
            Var "x_1" :& Var "x_2" :| Var "x_3"),

            (PrettyImpl $ [PrettyAnd $ [PrettyVar "x_1", PrettyVar "x_2"], PrettyVar "x_3"],
            Var "x_1" :& Var "x_2" :=> Var "x_3"),

            (PrettyImpl $ [PrettyOr $ [PrettyVar "x_1", PrettyVar "x_2"], PrettyVar "x_3"],
            Var "x_1" :| Var "x_2" :=> Var "x_3"),

            (PrettyEq $ [PrettyAnd $ [PrettyVar "x_1", PrettyVar "x_2"], PrettyVar "x_3"],
            Var "x_1" :& Var "x_2" :<=> Var "x_3"),

            (PrettyEq $ [PrettyOr $ [PrettyVar "x_1", PrettyVar "x_2"], PrettyVar "x_3"],
            Var "x_1" :| Var "x_2" :<=> Var "x_3"),

            (PrettyEq $ [PrettyImpl $ [PrettyVar "x_1", PrettyVar "x_2"], PrettyVar "x_3"],
            Var "x_1" :=> Var "x_2" :<=> Var "x_3"),

            (PrettyAnd $ [PrettyNot $ PrettyVar "x_1" , PrettyVar "x_2"],
            Not (Var "x_1") :& Var "x_2"),

            (PrettyOr $ [PrettyNot $ PrettyVar "x_1" , PrettyVar "x_2"],
            Not (Var "x_1") :| Var "x_2"),

            (PrettyImpl $ [PrettyNot $ PrettyVar "x_1" , PrettyVar "x_2"],
            Not (Var "x_1") :=> Var "x_2"),   

            (PrettyEq $ [PrettyNot $ PrettyVar "x_1" , PrettyVar "x_2"],
            Not (Var "x_1") :<=> Var "x_2") 
            ]

    assertEqualList error_message toPretty tests

testShow = TestCase $ do
    let error_message = "Show Expr unit test failed!"
    let tests = [
            ("x_1", Var $ "x_1"),
            ("x_1 & x_2", Var "x_1" :& Var "x_2")
            ] :: [(String, Expr)]
    assertEqualList error_message show tests


testExpr = TestList [testToPretty, testShow]
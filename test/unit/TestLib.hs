module TestLib where

import Test.HUnit
import Expr
import Lib

assertEqualList error_message f tests = do
    let (expected, input) = unzip tests
    let actual = map f input
    assertEqual error_message expected actual

assertEqualPair error_message f test = do
    let (expected, input) = test
    let actual = f input
    assertEqual error_message expected actual

testToBasis = TestCase $ do
    let error_message = "toBasic unit test failed!" 
    assertEqualPair error_message toBasis (Var "x_0", Var "x_0")
    assertEqualPair error_message toBasis (Not(Not (Var "x_0") :| Not (Var "x_1") :| Var "x_2"), Not(Var "x_0" :=> Var "x_1" :=> Var "x_2"))
    assertEqualPair error_message toBasis (read "~(x_0 & (~x_1 | ~x_2) | ~x_0 & ~(~x_1 | ~x_2))" , read "~(x_0 <=> x_1 => ~x_2)")

testToDML = TestCase $ do
    let error_message = "dml unit test failed!" 
    assertEqualPair error_message dml        (read "x_0", read "x_0")
    assertEqualPair error_message dml        (read "~x_0 | ~x_1 | ~x_2", read "~(x_0 & x_1 & x_2)")
    assertEqualPair error_message dml        (read "~x_0 & x_1 & x_2", read "~(x_0 | ~(x_1 & x_2))")
    assertEqualPair error_message dml        (read "x_0", read "~~~~~~~~x_0")

testToNNF = TestCase $ do
    let error_message = "toNNF unit test failed!" 
    assertEqualPair error_message toNNF        (read "x_0", read "x_0")
    assertEqualPair error_message toNNF        (read "x_0 | (x_1 & x_2 | ~x_1 & ~x_2)", read "~x_0 => (x_1 <=> x_2)")
    assertEqualPair error_message toNNF        (read "x_0 & ~x_1 | x_2", read "(x_0 => x_1) => x_2")

testToDNF = TestCase $ do
    let error_message = "toDNF unit test failed!" 
    assertEqualPair error_message toDNF        (read "x_0", read "x_0")
    assertEqualPair error_message toDNF        (read "~x_0 | x_1", read "x_0 => x_1")
    assertEqualPair error_message toDNF        (read "x_0 & x_1 | ~x_0 & ~x_1", read "x_0 <=> x_1")
    assertEqualPair error_message toDNF        (read "x_0 & x_1 | x_0 & x_2 | ~x_0 & ~x_1 & ~x_2", read "x_0 <=> (x_1 | x_2)")

testTseytin = TestCase $ do
    let error_message = "tseytin unit test failed!"  
    assertEqualPair error_message tseytin        (read "ts_0 & (ts_0 <=> x_0)", read "x_0")
    assertEqualPair error_message tseytin        (read "ts_0 & (ts_0 <=> ~x_0)", read "~x_0")
    assertEqualPair error_message tseytin        (read "ts_0 & (ts_1 <=> x_0 | x_1) & (ts_0 <=> ts_1 => x_2)", read "x_0 | x_1 => x_2")

testToCNF = TestCase $ do
    let error_message = "toCNF unit test failed!" 
    assertEqualPair error_message toCNF        (read "ts_0 & (ts_0 | ~ts_0) & (x_0 | ~ts_0) & (ts_0 | ~x_0) & (x_0 | ~x_0)", read "x_0")
    assertEqualPair error_message toCNF        (read "ts_0 & (ts_0 | ~ts_0) & (~x_1 | ~ts_0) & (ts_0 | x_1) & (~x_1 | x_1)", read "~x_1")

testLib = TestList [testToBasis, testToDML, testToNNF, testToDNF, testTseytin, testToCNF]
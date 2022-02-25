import Test.QuickCheck
import Expr
import Lib
import Checkers

instance Arbitrary Expr where
    arbitrary = sized arbitrary'
        where
        arbitrary' 0 = aVar 2
        arbitrary' n =
            oneof [ 
                  aVar $ round $ log $ fromIntegral n
                , (:|) <$> resize (n `div` 2) arbitrary <*> resize (n `div` 2) arbitrary
                , (:&) <$> resize (n `div` 2) arbitrary <*> resize (n `div` 2) arbitrary
                , (:=>) <$> resize (n `div` 2) arbitrary <*> resize (n `div` 2) arbitrary
                , (:<=>) <$> resize (n `div` 2) arbitrary <*> resize (n `div` 2) arbitrary
                , Not <$>  resize (n-1) arbitrary
                ]
        aVar sz = Var <$> ("x_"++) <$> show <$> resize sz (arbitrary :: Gen Int)

propShowRead :: Expr -> Property
propShowRead e = whenFail ( do
    putStr $ "Show-Read test failed"
    print $ show e
    print $ (\p -> showBr $ fst p) <$> readsBrExpr (show e)
    print $ show (read (show e) :: Expr)
    ) $ show e == show (read (show e) :: Expr)

---------------------------------------------

propEqNNF e = whenFail ( do
    putStr "EqNNF test failed"
    ) $ naiveEquivalence e $ toNNF e

propEqDNF e = whenFail ( do
    putStr "EqDNF test failed"
    ) $ naiveEquivalence e $ toDNF e

propEqCNF e = whenFail ( do
    putStr "EqCNF test failed"
    ) $ naiveEquivalence e $ toCNF e

propIsNNF e = whenFail ( do
    putStr "isNNF test failed"
    ) $ isNNF $ toNNF e

propIsDNF e = whenFail ( do
    putStr "isDNF test failed"
    ) $ isDNF $ toDNF e

propIsCNF e = whenFail ( do
    putStr "isCNF test failed"
    ) $ isCNF $ toCNF e

main :: IO ()
main = quickCheck $ withMaxSuccess 30  (
    propEqNNF .&&. 
    propEqDNF .&&. 
    propEqCNF .&&.
    propIsNNF .&&. 
    propIsDNF .&&.
    propIsCNF .&&.
    propShowRead 
    )




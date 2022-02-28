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

propEqCNFtseytin e = whenFail ( do
    putStr "EqCNF test failed"
    ) $ naiveEquivalence e $ toCNFtseytin e

propEqCNF1 e = whenFail ( do
    putStr "EqCNF test failed"
    ) $ naiveEquivalence e $ toCNF1 e

propEqCNFviaDNF e = whenFail ( do
    putStr "EqCNF test failed"
    ) $ naiveEquivalence e $ toCNFviaDNF e

propIsNNF e = whenFail ( do
    putStr "isNNF test failed"
    ) $ isNNF $ toNNF e

propIsDNF e = whenFail ( do
    putStr "isDNF test failed"
    ) $ isDNF $ toDNF e

propIsCNF e = whenFail ( do
    putStr "isCNF test failed"
    ) $ isCNF $ toCNF e

propIsCNFtseytin e = whenFail ( do
    putStr "isCNF test failed"
    ) $ isCNF $ toCNFtseytin e

propIsCNF1 e = whenFail ( do
    putStr "isCNF test failed"
    ) $ isCNF $ toCNF1 e

propIsCNFviaDNF e = whenFail ( do
    putStr "isCNF test failed"
    ) $ isCNF $ toCNFviaDNF e

main :: IO ()
main = do
    quickCheck $ withMaxSuccess 15  (
        propEqNNF .&&. 
        propEqDNF .&&. 
        -- propEqCNF .&&.
        -- propEqCNFtseytin .&&. 
        propEqCNF1 .&&.
        propEqCNFviaDNF .&&.
        propIsNNF .&&. 
        propIsDNF .&&.
        propIsCNF .&&.
        propIsCNFtseytin .&&.
        propIsCNF1 .&&.
        propIsCNFviaDNF .&&.
        propShowRead 
        )
    




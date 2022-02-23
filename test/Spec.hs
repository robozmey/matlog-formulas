import Test.QuickCheck
import System.Random
import Lib

instance Arbitrary Expr where
    arbitrary = sized arbitrary'
        where
        arbitrary' 0 = Var <$> (show <$> (arbitrary :: Gen Int))
        arbitrary' n =
            oneof [ 
                  (:|) <$> resize (n `div` 2) arbitrary <*> resize (n `div` 2) arbitrary
                , (:&) <$> resize (n `div` 2) arbitrary <*> resize (n `div` 2) arbitrary
                , (:=>) <$> resize (n `div` 2) arbitrary <*> resize (n `div` 2) arbitrary
                , (:<=>) <$> resize (n `div` 2) arbitrary <*> resize (n `div` 2) arbitrary
                , Not <$> arbitrary
                ]

prop1 :: Expr -> Property
prop1 e = whenFail ( do
    print $ show e
    print $ (\p -> showBr $ fst p) <$> readsBrExpr (show e)
    print $ show (read (show e) :: Expr)
    ) $ show e == show (read (show e) :: Expr)

prop2 e = toPretty e == (toPretty (read (show e) :: Expr))

main :: IO ()
main = quickCheck $ withMaxSuccess 1000 $ (prop1) .&&. (prop2)




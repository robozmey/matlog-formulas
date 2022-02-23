import Test.QuickCheck
import System.Random
import Lib

genExpr :: RandomGen g => g -> (Expr, g)
genExpr g = do
    let (op, g1) = randomR (0, 6 :: Int) g
    case op of
        0 -> let (e1, g2) = genExpr g1 in let (e2, g3) = genExpr g2 in (e1 :| e2, g3)
        1 -> let (e1, g2) = genExpr g1 in let (e2, g3) = genExpr g2 in (e1 :& e2, g3)
        2 -> let (e1, g2) = genExpr g1 in let (e2, g3) = genExpr g2 in (e1 :=> e2, g3)
        3 -> let (e1, g2) = genExpr g1 in let (e2, g3) = genExpr g2 in (e1 :<=> e2, g3)
        4 -> let (e1, g2) = genExpr g1 in (Not e1, g2)
        otherwise -> let (i, g2) = randomR (0, 10 :: Int) g1 in (Var $ "x_" ++ show i, g2)

genExprs :: RandomGen g => g -> [Expr]
genExprs g = let (e, g1) = genExpr g in let es = genExprs g1 in e : es

instance Arbitrary Expr where
    arbitrary = sized arbitrary'
        where
        arbitrary' 0 = pure $ Var "x_1"
        arbitrary' n =
            oneof [ 
                  (:|) <$> resize (n `div` 2) arbitrary <*> resize (n `div` 2) arbitrary
                , (:&) <$> resize (n `div` 2) arbitrary <*> resize (n `div` 2) arbitrary
                -- , (:=>) <$> resize (n `div` 2) arbitrary <*> resize (n `div` 2) arbitrary
                -- , (:<=>) <$> resize (n `div` 2) arbitrary <*> resize (n `div` 2) arbitrary
                , Not <$> arbitrary
                ]


examples = [
    Not $ (Var "x_0" :=> Var "x_1") :=>  (  (Var "x_2"  :=> (Var "x_5" :=> Var "x_6") :=> Var "x_4")),
    Not $ Not $ Var "x"
    ] 
    -- ++ let gen = mkStdGen 2021 in take 10 $ genExprs gen

prop_showread :: Expr -> Bool
prop_showread e = show e == show (read (show e) :: Expr)



main :: IO ()
main = quickCheck $ prop_showread




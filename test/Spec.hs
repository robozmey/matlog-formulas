import Test.QuickCheck
import Lib


examples = [
    Not $ (Var "x_0" :=> Var "x_1") :=>  (  (Var "x_2"  :=> (Var "x_5" :=> Var "x_6") :=> Var "x_4")),
    Not $ Not $ Var "x"
    ]




check :: Expr -> Bool
check e = show e == show (read (show e) :: Expr)

main :: IO ()
main = quickCheck $ all check examples




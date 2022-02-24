module Lib(toNNF, toDNF, toCNF, toBasis, dml, distr) where

import Expr(Expr(..))

toBasis :: Expr -> Expr
toBasis (e1 :=> e2) = toBasis (Not e1 :| e2)
toBasis (e1 :<=> e2) = toBasis (e1 :& e2 :| (Not e1 :& Not e2))
toBasis (e1 :| e2) = toBasis (e1) :| toBasis (e2)
toBasis (e1 :& e2) = toBasis (e1) :& toBasis (e2)
toBasis (Not e) = Not $ toBasis e
toBasis e@(Var s) = e

dml :: Expr -> Expr
dml (Not (e1 :| e2)) = dml (Not e1) :& dml (Not e2)
dml (Not (e1 :& e2)) = dml (Not e1) :| dml (Not e2)
dml (e1 :| e2) = dml (e1) :| dml (e2)
dml (e1 :& e2) = dml (e1) :& dml (e2)
dml (e1 :=> e2) = undefined
dml (e1 :<=> e2) = undefined
dml (Not (Not e)) = dml e
dml (Not e) = Not $ dml e
dml e@(Var s) = e

distr :: Expr -> Expr
distr (e1 :=> e2) = undefined
distr (e1 :<=> e2) = undefined
distr (e1 :| e2) = distr e1 :| distr e2
distr (e1 :& e2) = case expr of 
                        a :& (b :| c) -> distr (a :& b) :| distr (a :& c)
                        (a :| b) :& c -> distr (a :& c) :| distr (b :& c)
                        _ -> expr
    where e1' = distr e1
          e2' = distr e2
          expr = e1' :& e2'
distr e = e

toNNF :: Expr -> Expr
toNNF = dml . toBasis

toDNF :: Expr -> Expr
toDNF = distr . toNNF

toCNF :: Expr -> Expr
toCNF = dml . Not . toDNF . Not

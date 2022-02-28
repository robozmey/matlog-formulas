module Lib(toNNF, toDNF, toCNFtseytin, toCNF, toCNFviaDNF, toCNF1, tseytin, toBasis, dml, distrC, distrD) where

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

distrC :: Expr -> Expr
distrC (e1 :=> e2) = undefined
distrC (e1 :<=> e2) = undefined
distrC (e1 :| e2) = distrC e1 :| distrC e2
distrC (e1 :& e2) = case expr of 
                        a :& (b :| c) -> distrC (a :& b) :| distrC (a :& c)
                        (a :| b) :& c -> distrC (a :& c) :| distrC (b :& c)
                        _ -> expr
    where e1' = distrC e1
          e2' = distrC e2
          expr = e1' :& e2'
distrC e = e

distrD :: Expr -> Expr
distrD (e1 :=> e2) = undefined
distrD (e1 :<=> e2) = undefined
distrD (e1 :& e2) = distrD e1 :& distrD e2
distrD (e1 :| e2) = case expr of 
                        a :| (b :& c) -> distrD (a :| b) :& distrD (a :| c)
                        (a :& b) :| c -> distrD (a :| c) :& distrD (b :| c)
                        _ -> expr
    where e1' = distrD e1
          e2' = distrD e2
          expr = e1' :| e2'
distrD e = e

tseytin :: Expr -> Expr
tseytin expr = foldl (:&) (Var "ts_0") l
      where (l, _) = newVars 0 expr
            newVars :: Int -> Expr -> ([Expr], Int)                 
            newVars c0 (e1 :& e2) = (l1 ++ l2 ++ [Var ("ts_" ++ show c0) :<=> (e1 :& e2)], c2)
                  where (l1, c1) = newVars (c0+1) e1
                        (l2, c2) = newVars c1 e2
            newVars c0 (e1 :| e2) = (l1 ++ l2 ++ [Var ("ts_" ++ show c0) :<=> (e1 :| e2)], c2)
                  where (l1, c1) = newVars (c0+1) e1
                        (l2, c2) = newVars c1 e2
            newVars c0 (e1 :=> e2) = (l1 ++ l2 ++ [Var ("ts_" ++ show c0) :<=> (e1 :=> e2)], c2)
                  where (l1, c1) = newVars (c0+1) e1
                        (l2, c2) = newVars c1 e2
            newVars c0 (e1 :<=> e2) = (l1 ++ l2 ++ [Var ("ts_" ++ show c0) :<=> (e1 :<=> e2)], c2)
                  where (l1, c1) = newVars (c0+1) e1
                        (l2, c2) = newVars c1 e2
            newVars c0 (Not e) = (l1 ++ [Var ("ts_" ++ show c0) :<=> (Not e)], c1)
                  where (l1, c1) = newVars (c0+1) e
            newVars c0 (Var s) = ([Var ("ts_" ++ show c0) :<=> (Var s)], c0+1)
                  

toNNF :: Expr -> Expr
toNNF = dml . toBasis

toDNF :: Expr -> Expr
toDNF = distrC . toNNF

toCNF :: Expr -> Expr
toCNF = toCNFtseytin

toCNFtseytin :: Expr -> Expr
toCNFtseytin = distrD . toNNF . tseytin

toCNFviaDNF :: Expr -> Expr
toCNFviaDNF = dml . Not . toDNF . Not

toCNF1 :: Expr -> Expr
toCNF1 = distrD . toNNF

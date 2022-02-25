module Checkers where

import Data.Map
import Data.Set
import Expr
import Lib

----- ShowRead

showBr :: BrExpr -> String
showBr (e1 `BrAnd` e2) = "(" ++ showBr e1 ++ ") & (" ++ showBr e2 ++ ")"
showBr (e1 `BrOr` e2) = "(" ++ showBr e1 ++ ") | (" ++ showBr e2 ++ ")"
showBr (e1 `BrImpl` e2) = "(" ++ showBr e1  ++ ") => (" ++  showBr e2 ++ ")"
showBr (e1 `BrEq` e2) = "(" ++ showBr e1  ++ ") <=> (" ++  showBr e2 ++ ")"
showBr (BrVar s) = s
showBr (BrNot e) = "~(" ++ showBr e ++ ")"
showBr (BrBracket e) = "[" ++ showBr e ++ "]"

------ Equivalence

getVars :: Expr -> [Symb]
getVars (e1 :| e2) = getVars e1 ++ getVars e2
getVars (e1 :& e2) = getVars e1 ++ getVars e2
getVars (e1 :=> e2) = getVars e1 ++ getVars e2
getVars (e1 :<=> e2) = getVars e1 ++ getVars e2
getVars (Not e) = getVars e
getVars (Var s) = [s]

calculateExpr :: Map Symb Bool -> Expr -> Bool
calculateExpr mp (e1 :| e2) = calculateExpr mp e1 || calculateExpr mp e2
calculateExpr mp (e1 :& e2) = calculateExpr mp e1 && calculateExpr mp e2
calculateExpr mp (e1 :=> e2) = not (calculateExpr mp e1) || calculateExpr mp e2
calculateExpr mp (e1 :<=> e2) = calculateExpr mp e1 == calculateExpr mp e2
calculateExpr mp (Not e) = not $ calculateExpr mp e
calculateExpr mp (Var s) = mp ! s

enumerateBools :: Int -> [[Bool]]
enumerateBools n = do
    if n > 0 then do
        bs <- enumerateBools (n-1) :: [[Bool]]
        [bs ++ [False], bs ++ [True]]
    else
        [[]]

enumerateVars :: [Symb] -> [Map Symb Bool]
enumerateVars vs' = do
    let vs = Data.Set.toList $ Data.Set.fromList vs' :: [Symb]
    bs <- enumerateBools (length vs) :: [[Bool]]
    return $ Data.Map.fromList $ (zip vs bs :: [(Symb, Bool)])

naiveEquivalence :: Expr -> Expr -> Bool
naiveEquivalence e1 e2 = all id $ do
    let v1 = getVars e1
    let v2 = getVars e2
    mp <- enumerateVars $ v1 ++ v2 :: [Map Symb Bool]
    return $ calculateExpr mp e1 == calculateExpr mp e2

-------- Form confirmation

isNNF :: Expr -> Bool
isNNF (e1 :=> e2) = False
isNNF (e1 :<=> e2) = False
isNNF (e1 :| e2) = isNNF e1 && isNNF e2
isNNF (e1 :& e2) = isNNF e1 && isNNF e2
isNNF (Not (Var s)) = True
isNNF (Not _) = False
isNNF (Var s) = True

isDNF :: Expr -> Bool
isDNF (e1 :=> e2) = False
isDNF (e1 :<=> e2) = False
isDNF (e1 :| e2) = isDNF e1 && isDNF e2
isDNF e@(e1 :& e2) = isCClause e
isDNF (Not (Var s)) = True
isDNF (Not _) = False
isDNF (Var s) = True

isCClause :: Expr -> Bool
isCClause (e1 :=> e2) = False
isCClause (e1 :<=> e2) = False
isCClause (e1 :| e2) = False
isCClause (e1 :& e2) = isCClause e1 && isCClause e2
isCClause (Not (Var s)) = True
isCClause (Not _) = False
isCClause (Var s) = True

isCNF :: Expr -> Bool
isCNF (e1 :=> e2) = False
isCNF (e1 :<=> e2) = False
isCNF e@(e1 :| e2) = isDClause e
isCNF (e1 :& e2) = isCNF e1 && isCNF e2
isCNF (Not (Var s)) = True
isCNF (Not _) = False
isCNF (Var s) = True

isDClause :: Expr -> Bool
isDClause (e1 :=> e2) = False
isDClause (e1 :<=> e2) = False
isDClause (e1 :| e2) = isDClause e1 && isDClause e2
isDClause (e1 :& e2) = False
isDClause (Not (Var s)) = True
isDClause (Not _) = False
isDClause (Var s) = True

-----------------------------------------------------------------
heightExpr :: Expr -> Int
heightExpr (e1 :& e2) = max (heightExpr e1) (heightExpr e2) + 1
heightExpr (e1 :| e2) = max (heightExpr e1) (heightExpr e2) + 1
heightExpr (e1 :=> e2) = max (heightExpr e1) (heightExpr e2) + 1
heightExpr (e1 :<=> e2) = max (heightExpr e1) (heightExpr e2) + 1
heightExpr (Not e) = heightExpr e + 1
heightExpr (Var _) = 1
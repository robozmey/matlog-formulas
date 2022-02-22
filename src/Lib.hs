module Lib
    ( someFunc
    ) where


type Symb = String

infixl 5 :&
infixl 4 :|
infixr 3 :=>
infixl 2 :<=>

data Expr = 
    Var Symb | Expr :& Expr | Expr :| Expr | Expr :=> Expr | Expr :<=> Expr | Not Expr

data PrettyExpr = 
    PrettyVar Symb | PrettyAnd [PrettyExpr] | PrettyOr [PrettyExpr] | PrettyEq [PrettyExpr] | PrettyImpl [PrettyExpr] | PrettyNot PrettyExpr

instance Show Expr where
    showsPrec _ = showsPrettyExpr . toPretty

-- showsExpr :: Expr -> ShowS
-- showsExpr (Var s) = showString s
-- showsExpr (Not e) = showString "~" . showsExpr e
-- showsExpr (e1 :& e2) = showString "(". showsExpr e1 . showString " & " . showsExpr e2 . showString ")"
-- showsExpr (e1 :| e2) = showString "(". showsExpr e1 . showString " | " . showsExpr e2 . showString ")"
-- showsExpr (e1 :=> e2) = showString "(". showsExpr e1 . showString " => " . showsExpr e2 . showString ")"
-- showsExpr (e1 :<=> e2) = showString "(". showsExpr e1 . showString " <=> " . showsExpr e2 . showString ")"

toPretty :: Expr -> PrettyExpr
toPretty (Var s) = PrettyVar s
toPretty (Not e) = PrettyNot $ toPretty e
toPretty (e1 :| e2) = PrettyOr $ 
    let pe1 = toPretty e1 in 
    let pe2 = toPretty e2 in
    let es1 = case pe1 of
                PrettyOr es1' -> es1'
                otherwise -> [pe1]
    in
    let es2 = case pe2 of
                PrettyOr es2' -> es2'
                otherwise -> [pe2] 
    in
    es1 ++ es2
toPretty (e1 :& e2) = PrettyAnd $ 
    let pe1 = toPretty e1 in 
    let pe2 = toPretty e2 in
    let es1 = case pe1 of
                PrettyAnd es1' -> es1'
                otherwise -> [pe1]
    in
    let es2 = case pe2 of
                PrettyAnd es2' -> es2'
                otherwise -> [pe2] 
    in
    es1 ++ es2
toPretty (e1 :=> e2) = PrettyImpl $ 
    let pe1 = toPretty e1 in 
    let pe2 = toPretty e2 in
    let es1 = case pe1 of
                PrettyImpl es1' -> es1'
                otherwise -> [pe1]
    in
    let es2 = case pe2 of
                PrettyImpl es2' -> es2'
                otherwise -> [pe2] 
    in
    es1 ++ es2
toPretty (e1 :<=> e2) = PrettyEq $ 
    let pe1 = toPretty e1 in 
    let pe2 = toPretty e2 in
    let es1 = case pe1 of
                PrettyEq es1' -> es1'
                otherwise -> [pe1]
    in
    let es2 = case pe2 of
                PrettyEq es2' -> es2'
                otherwise -> [pe2] 
    in
    es1 ++ es2

addBrackets :: ShowS -> ShowS
addBrackets ss = showString "(". ss . showString ")"

showsPrettyExpr :: PrettyExpr -> ShowS
showsPrettyExpr (PrettyVar s) = showString s
showsPrettyExpr (PrettyNot e) = showString "~ " . showsPrettyExprBr e
showsPrettyExpr (PrettyAnd []) = undefined
showsPrettyExpr (PrettyAnd (e:es)) = foldl (\s e -> s . showString " & " . showsPrettyExprBr e) (showsPrettyExprBr e) es
showsPrettyExpr (PrettyOr []) = undefined
showsPrettyExpr (PrettyOr (e:es)) = foldl (\s e -> s . showString " | " . showsPrettyExprInOr e) (showsPrettyExprInOr e) es
showsPrettyExpr (PrettyImpl []) = undefined
showsPrettyExpr (PrettyImpl (e:es)) = foldl (\s e -> s . showString " | " . showsPrettyExprInImpl e) (showsPrettyExprInImpl e) es
showsPrettyExpr (PrettyEq []) = undefined
showsPrettyExpr (PrettyEq (e:es)) = foldl (\s e -> s . showString " | " . showsPrettyExprInEq e) (showsPrettyExprInEq e) es

showsPrettyExprInOr :: PrettyExpr -> ShowS
showsPrettyExprInOr e@(PrettyAnd _) = showsPrettyExpr e
showsPrettyExprInOr e = showsPrettyExprBr e

showsPrettyExprInImpl :: PrettyExpr -> ShowS
showsPrettyExprInImpl e@(PrettyAnd _) = showsPrettyExpr e
showsPrettyExprInImpl e@(PrettyOr _) = showsPrettyExpr e
showsPrettyExprInImpl e = showsPrettyExprBr e

showsPrettyExprInEq :: PrettyExpr -> ShowS
showsPrettyExprInEq e@(PrettyAnd _) = showsPrettyExpr e
showsPrettyExprInEq e@(PrettyOr _) = showsPrettyExpr e
showsPrettyExprInEq e@(PrettyImpl _) = showsPrettyExpr e
showsPrettyExprInEq e = showsPrettyExprBr e

showsPrettyExprBr :: PrettyExpr -> ShowS
showsPrettyExprBr e@(PrettyVar _) = showsPrettyExpr e
showsPrettyExprBr e@(PrettyNot _) = showsPrettyExpr e
showsPrettyExprBr e@(PrettyAnd _) = addBrackets $ showsPrettyExpr e
showsPrettyExprBr e@(PrettyOr _) = addBrackets $ showsPrettyExpr e

someFunc :: IO ()
someFunc = print (Not $ Var "x_1" :|  (  (Var "x_2" :| Var "x_4" :& (Var "x_5" :| Var "x_6"))))



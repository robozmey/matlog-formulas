module Lib where

import Control.Applicative


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
    let es1 = [pe1] in
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
showsPrettyExpr (PrettyImpl (e:es)) = foldl (\s e -> s . showString " => " . showsPrettyExprInImpl e) (showsPrettyExprInImpl e) es
showsPrettyExpr (PrettyEq []) = undefined
showsPrettyExpr (PrettyEq (e:es)) = foldl (\s e -> s . showString " <=> " . showsPrettyExprInEq e) (showsPrettyExprInEq e) es

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
showsPrettyExprBr e = addBrackets $ showsPrettyExpr e


instance Read Expr where
    readsPrec _ = readsExpr
    
isSeparator :: Char -> Bool 
isSeparator c = any (== c) " ()|&<=~"

isSpace :: Char -> Bool 
isSpace = (== ' ')

isBracket :: Char -> Bool 
isBracket c = any (== c) "()"

isOperator :: String -> Bool
isOperator op = any (== op) ["&", "|", "~", "=>", "<=>"]

getOperator :: String -> (Expr -> Expr -> Expr)
getOperator "|" = (:|)
getOperator "&" = (:&)
getOperator "=>" = (:=>)
getOperator "<=>" = (:<=>)


takeBracket :: String -> String
takeBracket = takeBracket' 0
takeBracket' :: Int -> String -> String
takeBracket' 0 ('(':s) = takeBracket' 1 s
takeBracket' 1 (')':s) = ""
takeBracket' l (')':s) = ')' : takeBracket' (l-1) s
takeBracket' l ('(':s) = '(' : takeBracket' (l+1) s
takeBracket' l (c:s) = c : takeBracket' l s
takeBracket' _ [] = undefined

dropBracket :: String -> String
dropBracket = dropBracket' 0
dropBracket' :: Int -> String -> String
dropBracket' 0 ('(':s) = dropBracket' 1 s
dropBracket' 1 (')':s) = s
dropBracket' l (')':s) = dropBracket' (l-1) s
dropBracket' l ('(':s) = dropBracket' (l+1) s
dropBracket' l (_:s) = dropBracket' l s
dropBracket' _ [] = undefined

takeOperator :: String -> String
takeOperator s = takeWhile isSeparator s

dropOperator :: String -> String
dropOperator s = dropWhile isSeparator s

getFirstOp :: String -> (String,String,String)
getFirstOp s = (case lex s of
            [("(",_)]   -> case lex (dropBracket s) of
                [(s2'', s3'')] -> 
                    if s2'' ++ s3'' == "" then getFirstOp (takeBracket s) else (takeBracket s, s2'', s3'')
                otherwise -> ( "LOL", "", "")
            [("~",s2')] -> (s2', "~", "")
            [(s1', s2')] -> case lex s2' of
                [(s2'', s3'')] -> (s1', s2'', s3'')
                otherwise -> ( "LOL2", "", "")
            otherwise -> ( "LOL3", "", ""))

readsExpr :: ReadS (Expr)
readsExpr s' = do
    let s = filter (/=' ') s'
    let (s1, op, s2) = getFirstOp s
    case op ++ s2 of
        "" -> return (Var s1, "")
        otherwise -> do
            (e11, s12) <- (reads s1) :: [(Expr, String)]
            case op of
                "~" -> return (Not e11, s12)
                otherwise -> do
                    (e21, s22) <- (reads s2) :: [(Expr, String)]
                    case s12 of 
                        "" -> if isOperator op then  return  ((getOperator op) e11 e21,s22) else return (e11, s12)
                        otherwise -> []
        

        
    




someFunc :: IO ()
someFunc = print y

y = (read x :: Expr)
x1 = "x_0=>x_1"
x2 = "(x_0=>x_1)=>x_2=>(x_5=>x_6)=>x_4"
x = show (Not $ (Var "x_0" :=> Var "x_1") :=>  (  (Var "x_2"  :=> (Var "x_5" :=> Var "x_6") :=> Var "x_4")))



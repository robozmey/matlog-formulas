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
    -- showsPrec _ = showsExpr

showsExpr :: Expr -> ShowS
showsExpr (Var s) = showString s
showsExpr (Not e) = showString "~" . showsExpr e
showsExpr (e1 :& e2) = showString "(". showsExpr e1 . showString " & " . showsExpr e2 . showString ")"
showsExpr (e1 :| e2) = showString "(". showsExpr e1 . showString " | " . showsExpr e2 . showString ")"
showsExpr (e1 :=> e2) = showString "(". showsExpr e1 . showString " => " . showsExpr e2 . showString ")"
showsExpr (e1 :<=> e2) = showString "(". showsExpr e1 . showString " <=> " . showsExpr e2 . showString ")"

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
showsPrettyExpr (PrettyNot e) = showString "~" . showsPrettyExprBr e
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

flipExpr (e1 :& (e2 :| e3)) = (e1 :& e2 :| e3)
flipExpr (e1 :| (e2 :=> e3)) = (e1 :| e2 :=> e3)
flipExpr (e1 :& (e2 :=> e3)) = (e1 :& e2 :=> e3)
flipExpr (e1 :=> (e2 :<=> e3)) = (e1 :=> e2 :<=> e3)
flipExpr (e1 :| (e2 :<=> e3)) = (e1 :| e2 :<=> e3)
flipExpr (e1 :& (e2 :<=> e3)) = (e1 :& e2 :<=> e3)
flipExpr e = e

dropOperator :: String -> String
dropOperator s = dropWhile isSeparator s

mylex ('~':s) = [("~", s)]
mylex ('(':s) = [("(", s)]
mylex ('|':s) = [("|", s)]
mylex ('&':s) = [("&", s)]
mylex ('=':'>':s) = [("=>", s)]
mylex ('<':'=':'>':s) = [("<=>", s)]
mylex (' ':s) = mylex s
mylex [] = [("", "")]
mylex (c:s) = do
    (s1, s2) <- mylex s
    if isOperator s1 then return (c:"", s)
    else return (c:s1, s2)

-- ffff :: (String,String,String, Bool) -> (String,String,String, Bool)
data BracketPosition = BothBracket | LeftBracket | NoneBracket 
    deriving Eq
ffff [(a, b, c, _)] = [(a, b, c, BothBracket)]
ffff [] = []

getFirstOp :: String -> [(String,String,String, BracketPosition)]
getFirstOp s = do
    (s1', s2') <- mylex s
    case (s1', s2') of
            ("(",_) -> do 
                (s2'', s3'') <- mylex (dropBracket s)
                if s2'' ++ s3'' == "" then ffff (getFirstOp (takeBracket s)) else return (takeBracket s, s2'', s3'', LeftBracket)
            ("~",_) -> return (s2', "~", "", NoneBracket)
            _ -> do
                (s2'', s3'') <- mylex s2'
                return (s1', s2'', s3'', NoneBracket)


addNot k e = foldr (\_ v -> Not v) e [1..k]
addNotB k b e = if b then foldr (\_ v -> Not v) e [1..k] else e

readsExpr :: ReadS (Expr)
readsExpr s = map fst $ readsExpr' 0 s
readsExpr' :: Int -> (String -> [((Expr, String), Bool)])
readsExpr' k s = do
    (s1, op, s2, bracket_pos) <- getFirstOp s
    case op ++ s2 of
        "" -> return ((addNot k (Var s1), ""), bracket_pos == BothBracket)
        otherwise -> do
            case op of
                "~" -> do
                    if bracket_pos == BothBracket then do
                        ((e11, s12),_) <- (readsExpr' 1 s1) :: [((Expr, String), Bool)]
                        return ((addNot k e11, s12), bracket_pos == BothBracket)
                    else do
                        ((e11, s12),_) <- (readsExpr' (k+1) s1) :: [((Expr, String), Bool)]
                        return ((e11, s12), bracket_pos == BothBracket)
                otherwise -> do
                    ((e11', s12), _)<- (readsExpr' (if (bracket_pos /= NoneBracket) then 0 else k) s1) :: [((Expr, String), Bool)]
                    let e11 = (addNotB k (bracket_pos == LeftBracket) e11')
                    if isOperator op then do
                        ((e21, s22), isSecondBracket) <- (readsExpr' 0 s2) :: [((Expr, String), Bool)]

                        let res = (if not isSecondBracket then flipExpr else id) $ (getOperator op) e11 e21

                        return ((addNotB k (bracket_pos == BothBracket) res, s22), bracket_pos == BothBracket)
                        
                    else do
                        case s12 of
                            "" -> return ((e11, s12), bracket_pos == BothBracket)
                            otherwise -> []
        


someFunc :: IO ()
someFunc = print y

y = (read bracket_test :: Expr)
bracket_test = "1|(2&3&4)|5|6|7&8&9|0|~~10"
not_test = "~x | ~(x|x) | ~(x|(x|x)) | ~ ~x"
x2 = "(x_0=>x_1)=>x_2=>(x_5=>x_6)=>x_4"
x = show (Not $ Var "x_0" :| Var "x_1" :&  (  (Var "x_2"  :& Var "x_5" :| Var "x_6" :& Var "x_4")))


modif :: Expr -> Expr
modif (Var s) = Var $ "[" ++ s ++ "]"
modif (Not e) = Not $ modif e
modif (e1 :& e2) = (modif e1) :& (modif e2)
modif (e1 :| e2) = (modif e1) :| (modif e2)
modif (e1 :=> e2) = (modif e1) :=> (modif e2)
modif (e1 :<=> e2) = (modif e1) :<=> (modif e2)
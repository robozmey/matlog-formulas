module Lib where

import Control.Applicative

type Symb = String

infixl 5 :&
infixl 4 :|
infixr 3 :=>
infixl 2 :<=>

data Expr = 
    Var Symb | 
    Expr :& Expr | 
    Expr :| Expr | 
    Expr :=> Expr | 
    Expr :<=> Expr | 
    Not Expr 

data PrettyExpr = 
    PrettyVar Symb | 
    PrettyAnd [PrettyExpr] | 
    PrettyOr [PrettyExpr] | 
    PrettyEq [PrettyExpr] | 
    PrettyImpl [PrettyExpr] | 
    PrettyNot PrettyExpr
    deriving Eq

instance Show Expr where
    showsPrec _ = showsPrettyExpr . toPretty

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

--------------------------------------------------------------------------------------------------------------------------------
instance Read Expr where
    readsPrec _ s = map (\(e, s1) -> (fromBrExpr$ allFlip $ allFlip $ allFlip e, s1)) (readsBrExpr s)

isOperator :: String -> Bool
isOperator op = any (== op) ["&", "|", "~", "=>", "<=>"]

getOperator :: String -> (BrExpr -> BrExpr -> BrExpr)
getOperator "|" = BrOr
getOperator "&" = BrAnd
getOperator "=>" = BrImpl
getOperator "<=>" = BrEq

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
ffff [(a, b, c, d, _)] = [(a, b, c, d, True)]
ffff [] = []

getFirstOp :: String -> [(String,String,String, Bool, Bool)]
getFirstOp s = do
    (s1', s2') <- mylex s
    case (s1', s2') of
            ("(",_) -> do 
                (s2'', s3'') <- mylex (dropBracket s)
                if s2'' ++ s3'' == "" then ffff (getFirstOp (takeBracket s)) else return (takeBracket s, s2'', s3'', True, False)
            ("~",_) -> return (s2', "~", "", False, False)
            _ -> do
                (s2'', s3'') <- mylex s2'
                return (s1', s2'', s3'', False, False)


addNot k e = foldr (\_ v -> BrNot v) e [1..k]
addNotB k b e = if b then foldr (\_ v -> BrNot v) e [1..k] else e

addBr b e = if b then BrBracket e else e

readsBrExpr :: ReadS (BrExpr)
readsBrExpr s = map fst $ readsBrExpr' 0 s
readsBrExpr' :: Int -> (String -> [((BrExpr, String), Bool)])
readsBrExpr' k s = do
    (s1, op, s2, left_in_bracket, both_in_bracket) <- getFirstOp s
    case op ++ s2 of
        "" -> return ((addNot k (BrVar s1), ""), both_in_bracket)
        otherwise -> do
            case op of
                "~" -> do
                    if both_in_bracket then do
                        ((e11, s12),_) <- (readsBrExpr' 1 s1) :: [((BrExpr, String), Bool)]
                        return ((addBr (both_in_bracket) $ addNot k e11, s12), True)
                    else do
                        ((e11, s12),_) <- (readsBrExpr' (k+1) s1) :: [((BrExpr, String), Bool)]
                        return ((e11, s12), False)
                otherwise -> do
                    ((e11', s12), _)<- (readsBrExpr' (if (both_in_bracket || left_in_bracket) then 0 else k) s1) :: [((BrExpr, String), Bool)]
                    let e11 = addBr left_in_bracket (addNotB k (left_in_bracket && not both_in_bracket) e11')
                    if isOperator op then do
                        ((e21, s22), is_second_bracket) <- (readsBrExpr' 0 s2) :: [((BrExpr, String), Bool)]

                        let res = (getOperator op) e11 e21

                        return ((addBr both_in_bracket (addNotB k both_in_bracket res), s22), both_in_bracket)
                        
                    else do
                        case s12 of
                            "" -> return ((e11, s12), both_in_bracket)
                            otherwise -> []

data BrExpr = 
    BrVar Symb | 
    BrAnd BrExpr BrExpr | 
    BrOr BrExpr BrExpr | 
    BrImpl BrExpr BrExpr | 
    BrEq BrExpr BrExpr | 
    BrNot BrExpr |
    BrBracket BrExpr 
    deriving Show

oneFlip :: BrExpr -> BrExpr
oneFlip (e1 `BrAnd` (e2 `BrOr` e3))   =  ((e1 `BrAnd` e2) `BrOr` e3)     --
oneFlip (e1 `BrAnd` (e2 `BrImpl` e3)) =  ((e1 `BrAnd` e2) `BrImpl` e3) --
oneFlip (e1 `BrOr` (e2 `BrImpl` e3))  =  ((e1 `BrOr` e2) `BrImpl` e3)
oneFlip (e1 `BrAnd` (e2 `BrEq` e3))   =  ((e1 `BrAnd` e2) `BrEq` e3)     --
oneFlip (e1 `BrOr` (e2 `BrEq` e3))    =  ((e1 `BrOr` e2) `BrEq` e3)
oneFlip (e1 `BrImpl` (e2 `BrEq` e3))  =  ((e1 `BrImpl` e2) `BrEq` e3)
oneFlip ((e1 `BrOr` e2) `BrAnd` e3)   =  (e1 `BrOr` (e2 `BrAnd` e3) )     --
oneFlip ((e1 `BrImpl` e2) `BrAnd` e3) =  (e1 `BrImpl` (e2 `BrAnd` e3) ) --
oneFlip ((e1 `BrImpl` e2) `BrOr` e3)  =  (e1 `BrImpl` (e2 `BrOr` e3) )
oneFlip ((e1 `BrEq` e2) `BrAnd` e3)   =  (e1 `BrEq` (e2 `BrAnd` e3))     --
oneFlip ((e1 `BrEq` e2) `BrOr` e3)    =  (e1 `BrEq` (e2 `BrOr` e3))
oneFlip ((e1 `BrEq` e2) `BrImpl` e3)  =  (e1 `BrEq` (e2 `BrImpl` e3))
oneFlip e = e

allFlip :: BrExpr -> BrExpr
allFlip (e1 `BrAnd` e2) = oneFlip $ allFlip e1 `BrAnd` allFlip e2
allFlip (e1 `BrOr` e2) = oneFlip $ allFlip e1 `BrOr` allFlip e2
allFlip (e1 `BrImpl` e2) = oneFlip $ allFlip e1 `BrImpl` allFlip e2
allFlip (e1 `BrEq` e2) = oneFlip $ allFlip e1 `BrEq` allFlip e2
allFlip (BrBracket e) = BrBracket $ allFlip e
allFlip (BrNot e) = BrNot $ allFlip e
allFlip e@(BrVar s) = e

fromBrExpr :: BrExpr -> Expr
fromBrExpr (e1 `BrAnd` e2) = fromBrExpr e1 :& fromBrExpr e2
fromBrExpr (e1 `BrOr` e2) = fromBrExpr e1 :| fromBrExpr e2
fromBrExpr (e1 `BrImpl` e2) = fromBrExpr e1 :=> fromBrExpr e2
fromBrExpr (e1 `BrEq` e2) = fromBrExpr e1 :<=> fromBrExpr e2
fromBrExpr (BrVar s) = Var s
fromBrExpr (BrNot e) = Not $ fromBrExpr e
fromBrExpr (BrBracket e) = fromBrExpr e

showBr :: BrExpr -> String
showBr (e1 `BrAnd` e2) = "(" ++ showBr e1 ++ ") & (" ++ showBr e2 ++ ")"
showBr (e1 `BrOr` e2) = "(" ++ showBr e1 ++ ") | (" ++ showBr e2 ++ ")"
showBr (e1 `BrImpl` e2) = "(" ++ showBr e1  ++ ") => (" ++  showBr e2 ++ ")"
showBr (e1 `BrEq` e2) = "(" ++ showBr e1  ++ ") <=> (" ++  showBr e2 ++ ")"
showBr (BrVar s) = s
showBr (BrNot e) = "~(" ++ showBr e ++ ")"
showBr (BrBracket e) = "[" ++ showBr e ++ "]"

someFunc :: IO ()
someFunc = print ""

negationNormalForm :: Expr -> Expr
negationNormalForm (Not (e1 :| e2)) = negationNormalForm (Not e1) :& negationNormalForm (Not e2)
negationNormalForm (Not (e1 :& e2)) = negationNormalForm (Not e1) :| negationNormalForm (Not e2)
negationNormalForm (Not (e1 :=> e2)) = negationNormalForm (Not $ (Not e1 :| e2))
negationNormalForm (Not (e1 :<=> e2)) = negationNormalForm (Not $ (Not e1 :& e2 :| e1 :& Not e2))
negationNormalForm (Not (Not e)) = e
negationNormalForm (e1 :| e2) = negationNormalForm (e1) :| negationNormalForm (e2)
negationNormalForm (e1 :& e2) = negationNormalForm (e1) :& negationNormalForm (e2)
negationNormalForm (e1 :=> e2) = negationNormalForm (Not e1 :| e2)
negationNormalForm (e1 :<=> e2) = negationNormalForm (Not e1 :& e2 :| e1 :& Not e2)
negationNormalForm e = e

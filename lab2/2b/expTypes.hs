import Data.List
import Data.Char

type Name = String
type Domain = [Integer]
type Valuation = [(Name, Integer)]

data Expr = Val Integer
          | Var Name
          | Expr :+: Expr
          | Expr :-: Expr
          | Expr :*: Expr
          | Expr :/: Expr
          | Expr :%: Expr
          | Empty
          
par :: String -> String
par s = "(" ++ s ++ ")"

instance Show Expr where
  show (Val x) = show x
  show (Var x) = x
  show (p :+: q) = par(show p ++ " + " ++ show q)
  show (p :-: q) = par(show p ++ " - " ++ show q)
  show (p :*: q) = par(show p ++ " * " ++ show q)
  show (p :/: q) = par(show p ++ " / " ++ show q)
  show (p :%: q) = par(show p ++ " % " ++ show q)
  
vars :: Expr -> [Name]
vars p = sort (vars2 p)

vars2 :: Expr -> [Name]
vars2 (Val x) = []
vars2 (Var x) = [x]
vars2 (p :+: q) = uniq (vars2 p ++ vars2 q)
vars2 (p :-: q) = uniq (vars2 p ++ vars2 q)
vars2 (p :*: q) = uniq (vars2 p ++ vars2 q)
vars2 (p :/: q) = uniq (vars2 p ++ vars2 q)
vars2 (p :%: q) = uniq (vars2 p ++ vars2 q)

uniq :: Eq a => [a] -> [a]
uniq [] = []
uniq (x:xs) = x:uniq (filter (/=x) xs)

evalExpr :: Expr -> Valuation -> Integer
evalExpr (Val x) _ = x
evalExpr (Var x) v = sure(lookup x v) where sure (Just b) = b
evalExpr (p :+: q) v = (evalExpr p v) + (evalExpr q v)
evalExpr (p :-: q) v = (evalExpr p v) - (evalExpr q v)
evalExpr (p :*: q) v = (evalExpr p v) * (evalExpr q v)
evalExpr (p :/: q) v = (evalExpr p v) `div` (evalExpr q v)
evalExpr (p :%: q) v = (evalExpr p v) `mod` (evalExpr q v)

valuations :: [(Name,Domain)] -> [Valuation]
valuations [] = [[]]
valuations ((n,d):ns) = [ (n,x):v | x <- d, v <- valuations ns ]

pytriples :: Integer -> [Valuation]
pytriples n = checkTriples (valuations [("a",[1..10]),("b",[1..10]),("c",[1..10])])

checkTriples :: [Valuation] -> [Valuation]
checkTriples [] = []
checkTriples (v:vs)
  | checkTriple v = v : checkTriples vs
  | otherwise = checkTriples vs
  
checkTriple :: Valuation -> Bool
checkTriple v = ( (evalExpr ( ((Var "a") :*: (Var "a")) :+: ((Var "b") :*: (Var "b")) ) v) == (evalExpr ((Var "c") :*: (Var "c")) v) ) && ( (lookup "a" v) <= (lookup "b" v) )

tokenize :: String -> [String]
tokenize [] = []
tokenize (t:ts)
  | elem t "+-*/%()" = [t]:(tokenize ts)
  | isAlpha t = (t:takeWhile isAlpha ts): tokenize(dropWhile isAlpha ts)
  | isDigit t = (t:takeWhile isDigit ts): tokenize(dropWhile isDigit ts)
  | t == ' ' = tokenize ts
  | otherwise = error "Syntax Error: invalid character in input"
  
toExpr :: String -> Expr
toExpr str = parseE Empty (tokenize str)

parseE :: Expr -> [String] -> (Expr,[String])
parseE accepted tokens = parseE' acc rest
  where (acc, rest) = parseT accepted tokens ""
  
parseE' :: Expr -> [String] -> (Expr,[String])
parseE' accepted ("+":tokens) = parseE' acc rest
  where (acc, rest) = parseT accepted tokens "+"
parseE' accepted ("-":tokens) = parseE' acc rest
  where (acc, rest) = parseT accepted tokens "-"
parseE' accepted tokens = (accepted, tokens)

parseT :: Expr -> [String] -> String -> (Expr,[String])
--parseT accepted tokens [] = parseT' acc rest
--  where (acc, rest) = parseF accepted tokens
parseT accepted tokens sign =  parseT' acc rest
  where (acc, rest) = parseF accepted tokens sign
  
parseT' :: Expr -> [String] -> (Expr,[String])
parseT' accepted ("*":tokens) = parseT' acc rest
  where (acc, rest) = parseF accepted tokens "*"
parseT' accepted ("/":tokens) = parseT' acc rest
  where (acc, rest) = parseF accepted tokens "/"
parseT' accepted ("%":tokens) = parseT' acc rest
  where (acc, rest) = parseF accepted tokens "%"
parseT' accepted tokens = (accepted, tokens)

parseF :: Expr -> [String] -> String -> (Expr,[String])
parseF accepted ((t:ts):tokens) "+" 
  | t == '(' = ( accepted :+: exp, rest)
      where (exp, rest) = parseE Empty tokens
  | isAlpha t = (accepted :+: (Var (t:ts)), tokens)
  | isDigit t = (accepted :+: (Val (read (t:ts) :: Int)), tokens)
parseF accepted ((t:ts):tokens) "-" 
  | t == '(' = ( accepted :-: exp, rest)
    where (exp, rest) = parseE Empty tokens
  | isAlpha t = (accepted :-: (Var (t:ts)), tokens)
  | isDigit t = (accepted :-: (Val (read (t:ts) :: Int)), tokens)
parseF accepted ((t:ts):tokens) "*" 
  | t == '(' = ( accepted :*: exp, rest)
    where (exp, rest) = parseE Empty tokens
  | isAlpha t = (accepted :*: (Var (t:ts)), tokens)
  | isDigit t = (accepted :*: (Val (read (t:ts) :: Int)), tokens)
parseF accepted ((t:ts):tokens) "/" 
  | t == '(' = ( accepted :/: exp, rest)
    where (exp, rest) = parseE Empty tokens
  | isAlpha t = (accepted :/: (Var (t:ts)), tokens)
  | isDigit t = (accepted :/: (Val (read (t:ts) :: Int)), tokens)
parseF accepted ((t:ts):tokens) "%" 
  | t == '(' = ( accepted :%: exp, rest)
    where (exp, rest) = parseE Empty tokens
  | isAlpha t = (accepted :%: (Var (t:ts)), tokens)
  | isDigit t = (accepted :%: (Val (read (t:ts) :: Int)), tokens)

    

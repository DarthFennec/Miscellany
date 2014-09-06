module Main (main) where

import Data.List
import Data.Char
import Data.Maybe
import Control.Applicative
import Control.Monad

data OpType = One | Both
data Direction = DLeft | DRight | DNone deriving (Eq)

data Operator = Op
  { oper :: String
  , arit :: Int
  , prec :: Int
  , assc :: Direction
  , defn :: Bool -> BTree -> STree }

data Token = TVar String
           | TUnOp Operator
           | TBinOp Operator
           | TLParen | TRParen

data BTree = BinOp Operator BTree BTree
           | UnOp Operator BTree
           | Var String

data STree = Oper OpType STree STree
           | Symb Bool String

lparen :: Operator
lparen = Op [] 0 0 DNone (\_ _ -> Symb True [])

opAnd, opOr, opNot :: Bool -> BTree -> STree
opAnd True (BinOp _ x y) = Oper Both (runDef True x) (runDef True y)
opAnd False (BinOp _ x y) = Oper One (runDef False x) (runDef False y)
opOr True (BinOp _ x y) = Oper One (runDef True x) (runDef True y)
opOr False (BinOp _ x y) = Oper Both (runDef False x) (runDef False y)
opNot k (UnOp _ x) = runDef (not k) x

opList :: [Operator]
opList =
  [ Op "*" 2 2 DRight opAnd
  , Op "+" 2 1 DRight opOr
  , Op "~" 1 3 DNone opNot ]

runDef :: Bool -> BTree -> STree
runDef k x@(BinOp n _ _) = defn n k x
runDef k x@(UnOp n _) = defn n k x
runDef k (Var n) = Symb k n

lexi :: String -> [Token]
lexi [] = []
lexi ('(':xs) = TLParen:lexi xs
lexi (')':xs) = TRParen:lexi xs
lexi ('_':xs) = let (t, s) = lexSymbol ('_':xs) in t:lexi s
lexi ('\'':xs) = let (t, s) = lexSymbol ('\'':xs) in t:lexi s
lexi (x:xs)
  | isSpace x = lexi xs
  | isAlpha x = let (t, s) = lexSymbol (x:xs) in t:lexi s
  | isPrint x = let (t, s) = lexOperator (x:xs) in t:lexi s

lexSymbol, lexOperator :: String -> (Token, String)
lexSymbol s = let (x, xs) = span f s in (TVar x, xs)
  where f c = elem c "\'_" || isAlphaNum c
lexOperator s = let (x, xs) = break f s in (g x, xs)
  where f c = elem c "\'_()" || isAlphaNum c || isSpace c || not (isPrint c)
        h c = fromJust $ find (\n -> oper n == c) opList
        g c = case arit $ h c of 1 -> TUnOp $ h c
                                 2 -> TBinOp $ h c

parseAll :: [Token] -> BTree
parseAll ts = t
  where (Just (t, [])) = parse Nothing $ TLParen:ts ++ [TRParen]

parse :: Maybe (Operator, BTree) -> [Token] -> Maybe (BTree, [Token])
parse Nothing (TVar n:xs) = Just (Var n, xs)
parse Nothing (TLParen:xs) = Just (v, vs)
  where (Just (v, TRParen:vs)) = subparse lparen y ys
        (Just (y, ys)) = parse Nothing xs
parse Nothing (TUnOp n:xs) = Just (UnOp n v, vs)
  where (Just (v, vs)) = subparse n y ys
        (Just (y, ys)) = parse Nothing xs
parse (Just (t, a)) (TBinOp n:xs)
  | hasOpPrec t n = Nothing
  | otherwise = Just (BinOp n a v, vs)
  where (Just (v, vs)) = subparse n y ys
        (Just (y, ys)) = parse Nothing xs
parse (Just _) (TRParen:_) = Nothing

subparse :: Operator -> BTree -> [Token] -> Maybe (BTree, [Token])
subparse t v xs
  | isNothing prs = Just (v, xs)
  | otherwise = subparse t v' xs'
  where prs = parse (Just (t, v)) xs
        (Just (v', xs')) = prs

hasOpPrec :: Operator -> Operator -> Bool
hasOpPrec (Op _ _ p1 a1 _) (Op _ _ p2 a2 _)
  | p1 > p2 || p1 == p2 && a1 == DLeft && a2 == DLeft = True
  | p1 < p2 || p1 == p2 && a1 == DRight && a2 == DRight = False

solve :: STree -> [[(String, Bool)]]
solve (Symb t s) = [[(s, t)]]
solve (Oper t x y) = k t (solve x) $ solve y
  where k One x = union x
        k Both x = f.map sort.liftA2 union x
        f = filter (\x -> all (\(s, t) -> notElem (s, not t) x) x)

solveDPLL :: STree -> [[(String, Bool)]]
solveDPLL t = buildDPLL (getVars t) (Right t)

getVars :: STree -> [String]
getVars (Symb _ n) = [n]
getVars (Oper _ x y) = union (getVars x) $ getVars y

buildDPLL :: [String] -> Either Bool STree -> [[(String, Bool)]]
buildDPLL _ (Left True) = [[]]
buildDPLL _ (Left False) = []
buildDPLL (x:xs) (Right t) = union (f True) $ f False
  where f n = map (union [(x, n)]).buildDPLL xs.simplifyDPLL t x $ n

simplifyDPLL :: STree -> String -> Bool -> Either Bool STree
simplifyDPLL t@(Symb j m) n k
  | m == n = Left $ j == k
  | otherwise = Right t
simplifyDPLL (Oper j x y) n k = g j (simplifyDPLL x n k) (simplifyDPLL y n k)
  where g _ (Right x') (Right y') = Right $ Oper j x' y'
        g Both (Left False) _ = Left False
        g Both _ (Left False) = Left False
        g Both (Left True) x' = x'
        g Both x' (Left True) = x'
        g One (Left False) x' = x'
        g One x' (Left False) = x'
        g One (Left True) _ = Left True
        g One _ (Left True) = Left True

printSolution :: [[(String, Bool)]] -> String
printSolution [] = "F: This is a contradiction (no solutions)\n"
printSolution xs | elem [] xs = "T: This is a tautology (all solutions)\n"
printSolution xs = unlines.map (intercalate ", ".map p) $ xs
  where p (n, t) = n ++ '=':show t

solveRepl :: (STree -> [[(String, Bool)]]) -> IO ()
solveRepl f = getLine >>= putStr.printSolution.f.runDef True .parseAll.lexi

main :: IO ()
main = forever $ solveRepl solve

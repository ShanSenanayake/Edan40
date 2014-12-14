import Prelude hiding (fail, return, iterate)
import Data.Char

data Expr = Num Int | Var String | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr


type Parser a = String -> Maybe (a,String)


semicolon :: Parser Char
semicolon cs = lit ';' cs

becomes :: Parser String
becomes = (twochars ? (==":="))

char :: Parser Char
char (x:xs) = Just(x,xs)
char [] = Nothing

fail :: Parser a
fail cs = Nothing

return :: a -> Parser a
return a cs = Just(a,cs)

infix 7 ?
(?) :: Parser a -> (a -> Bool) -> Parser a
(m ? p) cs =
	case m cs of
		Nothing -> Nothing
		Just(a,cs) -> if p a then Just(a,cs) else Nothing

digit :: Parser Char
digit cs = (char ? isDigit) cs

infixl 3 !
(!) :: Parser a -> Parser a -> Parser a
(m ! n) cs =
	case m cs of
		Nothing -> n cs
		mcs -> mcs

letter :: Parser Char
letter cs = (char ? isAlpha) cs

space :: Parser Char
space cs = (char ? isSpace) cs

alphanum :: Parser Char 
alphanum cs = (letter ! digit) cs

lit :: Char -> Parser Char
lit c = char ? (==c)


infixl 6 #
(#) :: Parser a -> Parser b -> Parser (a, b)
(m # n) cs =
	case m cs of
		Nothing -> Nothing
		Just(p, cs') ->
			case n cs' of
				Nothing -> Nothing
				Just(q, cs'') -> Just((p,q), cs'')

twochars :: Parser String
twochars = (char # char) >-> (\ (a,b) -> [a,b])


infixl 5 >->
(>->) :: Parser a -> (a -> b) -> Parser b
(m >-> k) cs =
	case m cs of
		Just(a,cs') -> Just(k a, cs')
		Nothing -> Nothing

digitVal :: Parser Int
digitVal = digit >-> digitToInt


charUpper :: Parser Char
charUpper =  letter >-> toUpper

sndchar :: Parser Char
sndchar = twochars >-> last

infixl 7 -#
(-#) :: Parser a -> Parser b -> Parser b
m -# k = (m # k) >-> (\(a,b) -> b)


infixl 7 #-
(#-) :: Parser a -> Parser b -> Parser a
m #- k = (m # k) >-> (\(a,b) -> a)


iterate :: Parser a -> Int -> Parser [a]
iterate m 0 = return []
iterate m i = m # iterate m (i-1) >-> cons

cons :: (a, [a]) -> [a]
cons (hd, tl) = hd:tl



iter :: Parser a -> Parser [a]
iter m = m # iter m >-> cons ! return []

letters :: Parser String
letters = letter # iter letter >-> cons


token :: Parser a -> Parser a
token m = m #- iter space

word :: Parser String
word = token letters


accept :: String -> Parser String
accept w = token (iterate char (length w) ? (==w))

infix 4 #>
(#>) :: Parser a -> (a -> Parser b) -> Parser b
(m #> k) cs =
	case m cs of
		Nothing -> Nothing
		Just(a,cs') -> k a cs'

double :: Parser Char
double = char #> lit


bldNumber :: Int -> Int -> Int
bldNumber n d = 10*n+d


number' :: Int -> Parser Int
number' n =
	digitVal >-> bldNumber n #> number' ! return n

number :: Parser Int
number = token (digitVal #> number')


--mulOp :: Parser ((Expr, Expr) -> Expr)
mulOp = lit '*' >-> (\_ -> Mul)
	! lit '/' >-> (\_ -> Div)



















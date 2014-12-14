module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
    Assignment String Expr.T |
    If Expr.T Statement Statement |
    Skip String|
    Block [Statement] |
    While Expr.T Statement |
    Read String |
    Write Expr.T 
    deriving Show

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

ifStatement = accept "if" -# Expr.parse #- require "then" # statement #- require "else" # statement >-> buildIf
buildIf ((e,s1),s2) = If e s1 s2

skip = accept "skip" -# require ";" >-> buildSkip
buildSkip c = Skip c

block = accept "begin" -# iter statement #- require "end" >-> buildBlock
buildBlock s = Block s

while = accept "while" -# Expr.parse #- require "do" # statement >-> buildWhile
buildWhile (e,s) = While e s

read1 = accept "read" -# word #- require ";" >-> buildRead
buildRead v = Read v

write = accept "write" -# Expr.parse #- require ";" >-> buildWrite
buildWrite v = Write v

statement = assignment ! ifStatement ! skip ! block ! while ! read1 ! write

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]

exec [] _ _ = []

exec (If cond thenStmts elseStmts: stmts) dict input = 
    if (Expr.value cond dict)>0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input

exec (Assignment var expr : stmts) dict input = exec stmts newDict input 
    where newDict = Dictionary.insert (var,(Expr.value expr dict)) dict

exec (Skip string:stmts) dict input = exec stmts dict input

exec (Block blockStmts:stmts) dict input = exec (blockStmts++stmts) dict input

exec (While expr whileStmts:stmts) dict input = 
	if (Expr.value expr dict)>0
	then exec (whileStmts:(While expr whileStmts):stmts) dict input
	else exec stmts dict input

exec (Read var: stmts) dict (i:input) = exec stmts newDict input 
	where newDict = Dictionary.insert (var,i) dict

exec (Write expr: stmts) dict input = (Expr.value expr dict):(exec stmts dict input)





toString' :: T -> String 
toString' (Assignment var expr) = var++" := "++(Expr.toString expr)++";\n"
toString'  (If cond thenStmts elseStmts) = "if " ++ (Expr.toString cond) ++ " then\n" ++ 
	(toString thenStmts) ++ "else\n" ++ (toString elseStmts)
toString' (Skip v) = "skip;\n"
toString' (Block blockStmts) = "begin\n" ++ (blockToString blockStmts) ++ "end\n"
toString' (While expr whileStmts) = "while " ++ (Expr.toString expr) ++ " do\n" ++ (toString whileStmts)
toString' (Read var) = "read " ++ var ++ ";\n"
toString' (Write expr) = "write " ++ (Expr.toString expr) ++ ";\n"


blockToString :: [T] -> String
blockToString [] = []
blockToString (s:stmts) = (toString s) ++ (blockToString stmts)
instance Parse Statement where
  parse = statement 

  toString = toString'

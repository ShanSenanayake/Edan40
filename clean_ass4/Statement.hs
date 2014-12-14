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
exec (If cond thenStmts elseStmts: stmts) dict input = 
    if (Expr.value cond dict)>0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input
--Kan vara fel då flera instanser av samma variabler förekommer i mappen
exec (Assignment var expr : stmts) dict input = exec stmts newDict input 
    where newDict = Dictionary.insert (var,(Expr.value expr dict)) dict

exec (Skip string:stmts) dict input = exec stmts dict input

exec (Block blockStmts:stmts) dict input = exec (blockStmts++stmts) dict input

exec (While expr whileStmts:stmts) dict input = exec (whileExec (Expr.value expr dict) whileStmts stmts) dict input



whileExec:: (Num a, Ord a) => a -> b -> [b] -> [b]
whileExec n loop end 
    | n>0 = loop:(whileExec (n-1) loop end)
    | otherwise = end

instance Parse Statement where
  parse = statement
  toString = error "Statement.toString not implemented"

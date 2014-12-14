module Program(T, parse, fromString, toString, exec) where
import Parser hiding (T)
import qualified Statement
import qualified Dictionary
import Prelude hiding (return, fail)
newtype T = Program [Statement.T] deriving Show -- to be defined

programParse = iter Statement.parse >-> buildStmtList
buildStmtList stmts = Program stmts

execute :: T -> [Integer] -> [Integer]
execute (Program stmts) list = Statement.exec stmts Dictionary.empty list


iterToString :: T -> String
iterToString (Program []) = ""
iterToString (Program (s:stmts)) = (Statement.toString s)++(iterToString (Program (stmts)))


instance Parse T where
  parse = programParse
  toString = iterToString
             
exec = execute
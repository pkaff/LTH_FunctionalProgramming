module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
    Assignment String Expr.T |
	Skip String |
	BeginEnd [Statement] |
	If Expr.T Statement Statement |
	While Expr.T Statement |
	Read String |
	Write Expr.T
    deriving Show

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

skip = accept "skip" #- require ";" >-> Skip

beginEnd = accept "begin" -# iter parse #- require "end" >-> BeginEnd

ifThenElse = accept "if" -# Expr.parse #- require "then" # parse 
	#- require "else" # parse >-> buildIf
buildIf ((e, ifS), elseS) = If e ifS elseS

while = accept "while" -# Expr.parse #- require "do" # parse >-> buildWhile
buildWhile (e, s) = While e s

readVar = accept "read" -# word #- require ";" >-> Read

writeVar = accept "write" -# Expr.parse #- require ";" >-> Write

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec [] _ _ = []
exec (If cond thenStmts elseStmts: stmts) dict input = 
    if (Expr.value cond dict) > 0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input
exec (While e stmt: stmts) dict input =
	if (Expr.value e dict) > 0 
	then exec (stmt: While e stmt: stmts) dict input
	else exec stmts dict input
exec (Read s: stmts) dict (i:input) = exec stmts (Dictionary.insert (s, i) dict) input
exec (Write e: stmts) dict input = (Expr.value e dict) : exec stmts dict input
exec (Assignment s e: stmts) dict input = exec stmts (Dictionary.insert (s, Expr.value e dict) dict) input
exec (BeginEnd xs: stmts) dict input = exec (xs ++ stmts) dict input
exec (Skip s: stmts) dict input = exec stmts dict input

instance Parse Statement where
  parse = assignment ! skip ! beginEnd ! ifThenElse ! while ! readVar ! writeVar
  toString (Assignment s e) = s ++ " := " ++ Expr.toString e ++ ";\n"
  toString (Skip s) = s ++ ";\n"
  toString (BeginEnd stmts) = "begin\n" ++ concat (map toString stmts) ++ "end\n"
  toString (If e ifS elseS) = "if " ++ Expr.toString e ++ " then\n" ++ toString ifS 
	++ "else\n" ++ toString elseS
  toString (While e s) = "while " ++ Expr.toString e ++ " do\n" ++ toString s
  toString (Read s) = "read " ++ s ++ ";\n"
  toString (Write e) = "write " ++ Expr.toString e ++ ";\n"
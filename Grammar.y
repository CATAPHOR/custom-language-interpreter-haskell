{
module Grammar where
import Tokens
import System.IO
}

%name parseCalc
%tokentype { Token }
%error { parseError }
%token
     loop    	{ TokenLoop }
     int     	{ TokenInt $$ }
     literal 	{ TokenLiteral $$ }
     add     	{ TokenAdd }
     sub     	{ TokenSub }
     mul     	{ TokenMul }
     div     	{ TokenDiv }
     buffer  	{ TokenBuffer }
     release 	{ TokenRelease }
     out     	{ TokenOut }
     setVar  	{ TokenSetVar }
     var     	{ TokenVariable }
     end     	{ TokenEnd }
     '('     	{ TokenLBrack }
     ')'     	{ TokenRBrack }

%%
Program : end          	{ End }
     | Command Program  { Line $1 $2 }

Command : out Exp        	{ Out $2 }
     | setVar int Exp 	{ SetVar $2 $3 }
     | loop           	{ Loop }
     | buffer Exp Exp 	{ Buffer $2 $3 }

Exp : int         	{ IntVal $1 }
     | release Exp Exp { Release $2 $3 }
     | literal     	{ IntVal $1 }
     | add Exp Exp 	{ Add $2 $3 }
     | sub Exp Exp 	{ Sub $2 $3 }
     | mul Exp Exp 	{ Mul $2 $3 }
     | div Exp Exp 	{ Div $2 $3 }
     | '(' Exp ')' 	{ $2 }
     | var int     	{ Var $2 }

{
parseError :: [Token] -> a
parseError _ = error ("Invalid syntax: Error encountered while parsing program")

data Program = Line Command Program
     | End
     deriving Show

data Command = Out Exp
     | Loop
     | Buffer Exp Exp
     | SetVar Int Exp
     deriving Show

data Exp = Add Exp Exp
     | Sub Exp Exp
     | Mul Exp Exp
     | Div Exp Exp
     | Release Exp Exp
     | IntVal Int
     | Var Int
     deriving Show
}

{
--module Parser where
module Main (main) where

import Lexer
}

%name parse
%tokentype { Token }
%error { parseError }

%token
    '+'          { TkMas }
--    '-'        { Operator '-' }
--    '*'        { Operator '*' }
--    '/'        { Operator '/' }
--    '('        { LParen }
--    ')'        { RParen }
--	"pi"	   { Pi }
--	"e"	   	   { E }
--    int        { Int $$ }
--	"sin" 	   { Funcion }
--%left  '+' '-'
--%right '*' '/'
--%right NEG

%%

EM  : EM '+' EM        { Plus  $1 $3 }
--     | EM '-' EM       { Minus $1 $3 }
--     | EM '*' EM        { Times $1 $3 }
--     | EM '/' EM        { Div   $1 $3 }
--     | '(' EM ')'        { Paren $2 }
--     | '-' EM %prec NEG  { Negate $2 }
--     | int                { Intg $1 }
--	 | "pi"				  { Pi }
--	 | "e"				  { E }
--	 | Funcion '(' EM ')' { Funcion $1 $3 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data EM =
      Plus EM EM
--    | Minus EM EM
--    | Times EM EM
--    | Div EM EM
--    | Paren EM
--    | Negate EM
--	| Pi
--	| E
--	| Funcion EM
--    deriving (Show)

main = do
    s <- getContents
    print $ lexer s
}   

{
module Parser where

import Lexer
}

%name parse
%tokentype { Token }
%error { parseError }


%token
    '+'        { Operator '+' }
    '-'        { Operator '-' }
    '*'        { Operator '*' }
    '/'        { Operator '/' }
    '('        { LParen }
    ')'        { RParen }
    int        { Int $$ }

%left  '+' '-'
%right '*' '/'
%right NEG

%%

Exp  : Exp '+' Exp        { Plus  $1 $3 }
     | Exp '-' Exp        { Minus $1 $3 }
     | Exp '*' Exp        { Times $1 $3 }
     | Exp '/' Exp        { Div   $1 $3 }
     | '(' Exp ')'        { Paren $2 }
     | '-' Exp %prec NEG  { Negate $2 }
     | int                { Intg $1 }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

data Exp =
      Plus Exp Exp
    | Minus Exp Exp
    | Times Exp Exp
    | Div Exp Exp
    | Paren Exp
    | Negate Exp
    | Intg Int
    deriving (Show)
}


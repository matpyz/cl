{
module Parser (parseProgram) where

import Token
import Lexer
import Syntax
}

%name happyParseProgram
%tokentype { Token }
%error { \_ -> error "syntax error" }
%token
  DECLARE { DECLARE }
  IN { IN }
  END { END }
  IF { IF }
  THEN { THEN }
  ELSE { ELSE }
  ENDIF { ENDIF }
  WHILE { WHILE }
  DO { DO }
  ENDWHILE { ENDWHILE }
  FOR { FOR }
  DOWN { DOWN }
  FROM { FROM }
  TO { TO }
  ENDFOR { ENDFOR }
  GET { GET }
  PUT { PUT }
  ':=' { EQUAL }
  ';' { SEMICOLON }
  '(' { LPAREN }
  ')' { RPAREN }
  arithop { ARITHOP $$ }
  relop { RELOP $$ }
  num { NUM $$ }
  pidentifier { ID $$ }

%%

program : DECLARE vdeclarations IN commands END { (reverse $2, reverse $4) }

vdeclarations : vdeclarations pidentifier { ($2, Nothing) : $1 }
              | vdeclarations pidentifier '(' num ')' { ($2, Just $4) : $1 }
              | { [] }

commands : commands command { $2 : $1 }
         | { [] }

command : identifier ':=' expression ';' { $1 := $3 }
        | IF condition THEN commands ENDIF { If $2 (reverse $4) [] }
        | IF condition THEN commands ELSE commands ENDIF { If $2 (reverse $4) (reverse $6) }
        | WHILE condition DO commands ENDWHILE { While $2 (reverse $4) }
        | FOR pidentifier down FROM value TO value DO commands ENDFOR { For $2 $3 $5 $7 (reverse $9) }
        | GET identifier ';' { Get $2 }
        | PUT value ';' { Put $2 }

down : { False }
     | DOWN { True }

expression : value { Val $1 }
           | value arithop value { Expr $1 $2 $3 }

condition : value relop value { ($1, $2, $3) }

value : num { Lit $1 }
      | identifier { Var $1 }

identifier : pidentifier index { ($1, $2) }

index : { NoIx }
      | '(' num ')' { LitIx $2 }
      | '(' pidentifier ')' { VarIx $2 }

{
parseProgram :: String -> Program
parseProgram = happyParseProgram . alexScanTokens
}
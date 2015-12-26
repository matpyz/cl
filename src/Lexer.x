{
module Lexer (alexScanTokens) where

import Token
}

%wrapper "basic"

$comment = [\r\n[^\]]]

tokens :-
 DECLARE|IN|END
 |IF|THEN|ELSE|ENDIF
 |WHILE|DO|ENDWHILE
 |FOR|DOWN|FROM|TO|ENDFOR
 |GET|PUT { read }
 ":=" { const EQUAL }
 ";" { const SEMICOLON }
 "(" { const LPAREN }
 ")" { const RPAREN }
 "+" { const $ ARITHOP Add }
 "-" { const $ ARITHOP Sub }
 "*" { const $ ARITHOP Mul }
 "/" { const $ ARITHOP Div }
 "%" { const $ ARITHOP Mod }
 ">=" { const $ RELOP (False, LT) }
 "!=" { const $ RELOP (False, EQ) }
 "<=" { const $ RELOP (False, GT) }
 "<" { const $ RELOP (True, LT) }
 "=" { const $ RELOP (True, EQ) }
 ">" { const $ RELOP (True, GT) }
 [0-9]+ { NUM . read }
 [_a-z]+ { ID }
 $white+ | "[" $comment* "]" ;

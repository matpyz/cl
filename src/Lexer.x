{
module Lexer (alexScanTokens) where

import Token
}

%wrapper "posn"

$comment = [\r\n[^\]]]

tokens :-
 DECLARE|IN|END
 |IF|THEN|ELSE|ENDIF
 |WHILE|DO|ENDWHILE
 |FOR|DOWN|FROM|TO|ENDFOR
 |GET|PUT { const read }
 ":=" { \_ _ -> EQUAL }
 ";" { \_ _ -> SEMICOLON }
 "(" { \_ _ -> LPAREN }
 ")" { \_ _ -> RPAREN }
 "+" { \_ _ -> ARITHOP Add }
 "-" { \_ _ -> ARITHOP Sub }
 "*" { \_ _ -> ARITHOP Mul }
 "/" { \_ _ -> ARITHOP Div }
 "%" { \_ _ -> ARITHOP Mod }
 ">=" { \_ _ -> RELOP (False, LT) }
 "!=" { \_ _ -> RELOP (False, EQ) }
 "<=" { \_ _ -> RELOP (False, GT) }
 "<" { \_ _ -> RELOP (True, LT) }
 "=" { \_ _ -> RELOP (True, EQ) }
 ">" { \_ _ -> RELOP (True, GT) }
 [0-9]+ { \_ s -> NUM (read s) }
 [_a-z]+ { \(AlexPn _ l c) s -> ID (l, c, s) }
 $white+ | "[" $comment* "]" ;

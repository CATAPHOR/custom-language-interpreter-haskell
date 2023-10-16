{ 
module Tokens where 
}

%wrapper "basic" 
$digit = 0-9     
-- digits 
$alpha = [a-zA-Z]    
-- alphabetic characters

tokens :-
  Loop                          { \s -> TokenLoop } 
  $white+                       ; 
  ">>".*                        ; 
  $digit+                       { \s -> TokenInt (read s) }
  \L$digit+                     { \s -> TokenLiteral (read (tail s)) } 
  Add                           { \s -> TokenAdd }
  Sub                           { \s -> TokenSub }
  Mul                           { \s -> TokenMul }
  Div                           { \s -> TokenDiv }
  Buffer                        { \s -> TokenBuffer}
  Out                           { \s -> TokenOut }
  SetVar                        { \s -> TokenSetVar }
  Release                       { \s -> TokenRelease }
  \(                            { \s -> TokenLBrack }
  \)                            { \s -> TokenRBrack }
  Var                           { \s -> TokenVariable }
  End                           { \s -> TokenEnd }
  \#                            ;
  \@                            ;
  \~                            ;

{ 
-- Each action has type :: String -> Token 
-- The token type: 
data Token = 
  TokenLoop            |
  TokenLiteral Int     |
  TokenInt Int         |
  TokenVariable        |
  TokenAdd             |
  TokenSub             |
  TokenMul             |
  TokenDiv             |
  TokenBuffer          |
  TokenOut             |
  TokenLBrack          |
  TokenRBrack          |
  TokenRelease         |
  TokenEnd             |
  TokenSetVar          
  deriving (Eq,Show) 
}

{
open Lexing
open Parser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}

let alpha = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let str = (alpha|digit) (alpha|digit|'_')*
let qstr = [^ '"']+
let whitespace = [' ' '\t']
let newline = '\r' | '\n' | "\r\n"
let id = alpha (alpha|digit|'_'|'\'')*


rule read =
  parse
  | whitespace { read lexbuf }
  | newline    { read lexbuf }
  (** Sections *)
  | "sort"     { SORT }
  | "cons"     { CONS }
  | "eqn"      { EQN }
  | "map"      { MAP }
  | "var"      { VAR }
  | "glob"     { GLOB }
  | "act"      { ACT }
  | "proc"     { PROC }
  (** Char *)
  | ':'        { COLON }
  | ','        { COMMA }
  | '{'        { L_BRACE }
  | '['        { L_BRACK }
  | '('        { L_PARAN }
  | '%'        { PERCENT }
  | '?'        { Q_MARK }
  | ')'        { R_PARAN }
  | ']'        { R_BRACK }
  | '}'        { R_BRACE }
  | ';'        { SEMICOLON }
  | '|'        { V_BAR }
  | '!'        { EXCLAIM }
  | '-'        { MINUS }
  | '+'        { PLUS }
  | '.'        { DOT }
  | '*'        { ASTERISK }
  | '@'        { AT }
  (** Infix *)
  | '='        { EQUAL }
  | "->"       { R_ARROW }
  | '#'        { HASH }
  | "||"       { D_BAR }
  | "||_"      { D_BAR_ }
  | "&&"       { D_AMP }
  | "=="       { D_EQUAL }
  | "!="       { EXCLAIM_EQUAL }
  | '>'        { GT }
  | ">="       { GTE }
  | '<'        { LT }
  | "<="       { LTE }
  | "in"       { IN }
  | "=>"       { R_FARROW }
  | "|>"       { SNOC }
  | "<|"       { CONS2 }
  | "<>"       { DIAMOND }
  | "<<"       { L_CHEVRON }
  (** Sort Words *)
  | "true"     { TRUE }
  | "false"    { FALSE }
  | "forall"   { FORALL }
  | "exists"   { EXISTS }
  | "lambda"   { LAMBDA }
  | "Bool"     { S_BOOL }
  | "Pos"      { S_POS }
  | "Nat"      { S_NAT }
  | "Int"      { S_INT }
  | "Real"     { S_REAL }
  | "List"     { S_LIST }
  | "Bag"      { S_BAG }
  | "Set"      { S_SET }
  | "FBag"     { S_FBAG }
  | "FSet"     { S_FSET }
  | "struct"   { STRUCT }
  | "delta"    { DELTA }
  | "tau"      { TAU }
  | "block"    { BLOCK }
  | "allow"    { ALLOW }
  | "hide"     { HIDE }
  | "rename"   { RENAME }
  | "comm"     { COMM }
  | "sum"      { SUM }
  (** data words *)
  | "whr"      { WHERE }
  | "end"      { END }
  | "dis"      { DIST }
  | digit+     { NUMBER (Lexing.lexeme lexbuf |> int_of_string) }
  | id         { ID (Lexing.lexeme lexbuf) }
  | str        { STR (Lexing.lexeme lexbuf) }
  | _          { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof        { EOF }

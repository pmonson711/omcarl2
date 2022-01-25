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
let id = alpha (alpha|digit|'_')*


rule read =
  parse
  | whitespace { read lexbuf }
  | newline    { read lexbuf }
  (** Sections *)
  | "sort"     { SORT }
  | "cons"     { CONS }
  | "eqn"      { EQN }
  | "glob"     { GLOB }
  | "act"      { ACT }
  | "proc"     { PROC }
  (** Char *)
  | ':'        { COLON }
  | ','        { COMMA }
  | '('        { L_PARAN }
  | '%'        { PERCENT }
  | '?'        { Q_MARK }
  | ')'        { R_PARAN }
  | ';'        { SEMICOLON }
  | "|"        { V_BAR }
  (** Infix *)
  | "->"       { R_ARROW }
  | "#"        { HASH }
  (** Sort Words *)
  | "Bool"     { S_BOOL }
  | "Pos"      { S_POS }
  | "Nat"      { S_NAT }
  | "Int"      { S_INT }
  | "Real"     { S_REAL }
  | "Bag"      { S_BAG }
  | "FBag"     { S_FBAG }
  | "FSet"     { S_FSET }
  | "struct"   { STRUCT }
  | digit      { NUMBER (Lexing.lexeme lexbuf |> int_of_string) }
  | id         { ID (Lexing.lexeme lexbuf) }
  | str        { STR (Lexing.lexeme lexbuf) }
  | _          { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof        { EOF }

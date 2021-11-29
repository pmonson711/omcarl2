{
open Lexing
open Tokens
  
exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let int = '-'? digit+
let id = (alpha) (alpha|digit|'_')*
let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule read_tokens =
  parse
  | whitespace { read_tokens lexbuf }
  | newline    { next_line lexbuf; read_tokens lexbuf }

  (* | "cons"     { CONS_SEC } *)
  (* | "sort"     { SORT_SEC } *)
  (* | "map"      { MAP_SEC } *)
  (* | "var"      { VAR_SEC } *)
  (* | "eqn"      { EQN_SEC } *)

(* SORT *)
  | "struct"   { STRUCT }
  | "Bool"     { BOOL }
  | "Int"      { INT }
  | "Nat"      { NAT }
  | "Pos"      { POS }
  | "Real"     { REAL }
  | "List"     { LIST }
  | "Set"      { SET }
  | "Bag"      { BAG }
  | "FSet"     { FSET }
  | "FBag"     { FBAG }

(* DATA *)
  | "true"     { TRUE }
  | "false"    { FALSE }
  | '['        { L_BRACE }
  | ']'        { R_BRACE }
  | '{'        { L_BRACK }
  | '}'        { R_BRACK }

  | '('        { LPARAN }
  | ')'        { RPARAN }
  | "->"       { RARROW }
  | '#'        { HASH }
  | ':'        { COLON }
  | ','        { COMMA }
  | '='        { EQUAL }
  | '|'        { BAR }
  | '?'        { QUESTION }
  | '!'        { EXCLAIM }
  | ';'        { SEMI_COlON }
  | '-'        { NEGATION }
  | "forall"   { FORALL }
  | '.'        { DOT }

  | id         { ID (Lexing.lexeme lexbuf) }
  | digit      { NUMBER (int_of_string (Lexing.lexeme lexbuf)) }

  | _          { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof        { EOF }

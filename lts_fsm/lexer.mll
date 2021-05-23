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
let whitespace = [' ' '\t']
let newline = '\r' | '\n' | "\r\n"

rule read = 
  parse
  | whitespace  { read lexbuf }
  | newline+    { next_line lexbuf; EOL }
  | "---"       { DIVIDER }
  | '('         { L_PARAM }
  | ')'         { R_PARAM }
  | '"'         { D_QUOTE }
  | digit+      { POS (int_of_string (Lexing.lexeme lexbuf)) }
  | str         { STR (Lexing.lexeme lexbuf) }
  | _           { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof         { EOF }

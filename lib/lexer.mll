{
open Lexing
open Parser
  
exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbug.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_p;
               pos_lnum = pos.pos_lnum + 1
    }
}
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let int = '-'? digit+
let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

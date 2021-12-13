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


rule read =
  parse
  | whitespace { read lexbuf }
  | newline+   { read_line lexbuf; EOL }
  | "des"      { DES }
  | '('        { L_PARAM }
  | ')'        { R_PARAM }
  | '"'        { read_string (Buffer.create 17) lexbuf }
  | str        { STR (Lexing.lexeme lexbuf) }
  | _          { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof        { EOF }

and read_string buf =
  parse
  | '"'        { Q_STR (Buffer.contents buf) }
  | '\\' '"'   { Buffer.add_char buf '"'; read_string buf lexbuf }
  | '\\' 'n'   { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'   { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'   { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf }
  | _          { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof        { raise (SyntaxError ("String is not terminated")) }

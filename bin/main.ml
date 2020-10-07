open Lexing
open Printf
open Lib_json

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Parser.prog Lexer.read lexbuf with
  | Lexer.SyntaxError msg ->
      fprintf stderr "%a: %s\n" print_position lexbuf msg;
      None
  | Parser.Error          ->
      fprintf stderr "%a: syntax error\n" print_position lexbuf;
      exit (-1)

let rec parse_and_print lexbuf =
  match parse_with_error lexbuf with
  | Some value ->
      printf "%a\n" Json.output_value value;
      parse_and_print lexbuf
  | None       -> ()

let loop filename () =
  let inx = open_in filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  parse_and_print lexbuf;
  close_in inx

open Cmdliner

let () =
  let pp_json f () = loop f () in
  let f = Arg.(value & pos 0 string "test1.json" & info []) in
  let json_t = Term.(const pp_json $ f $ const ()) in
  let info = Term.info "This is a test" in
  Term.exit @@ Term.eval (json_t, info)

module Mcrl2 = Mcrl2
module Lexer = Lexer
module Cons_parser = Cons_parser
module Data_parser = Data_parser
module Sort_parser = Sort_parser
module Map_parser = Map_parser
module Proc_parser = Proc_parser
module Eqn_parser = Eqn_parser
module Act_parser = Act_parser
module Glob_parser = Glob_parser
module Mcrl2_parser = Mcrl_parser

let basic_parse ~f str =
  let open Lexer in
  str |> Lexing.from_string |> f @@ read_tokens

let proc_parse = basic_parse ~f:Proc_parser.prog

let cons_parse = basic_parse ~f:Cons_parser.prog

let data_parse = basic_parse ~f:Data_parser.prog

let sort_parse = basic_parse ~f:Sort_parser.prog

let spec_parse = basic_parse ~f:Mcrl2_parser.prog

let glob_parse = basic_parse ~f:Glob_parser.prog

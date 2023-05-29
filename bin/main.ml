open Spoutnik

let version = "1.0"

let usage () =
  let _ =
    Printf.eprintf
      "Usage: %s [file]\n\
       \tRead a Spoutnik program from file (default is stdin)\n\
       %!"
      Sys.argv.(0)
  in
  exit 1

let interactive = ref true
let debug = ref false

let main () =
  let input_channel =
    match Array.length Sys.argv with
    | 1 -> stdin
    | 2 -> (
        match Sys.argv.(1) with
        | "-d" | "--debug" ->
            debug := true;
            stdin
        | name -> (
            interactive := false;
            try open_in name
            with _ ->
              Printf.eprintf "Opening %s failed\n%!" name;
              exit 1))
    | 3 -> (
        match (Sys.argv.(1), Sys.argv.(2)) with
        | ("-d" | "--debug"), ("-d" | "--debug") ->
            debug := true;
            stdin
        | ("-d" | "--debug"), name | name, ("-d" | "--debug") -> (
            debug := true;
            interactive := false;
            try open_in name
            with _ ->
              Printf.eprintf "Opening %s failed\n%!" name;
              exit 1)
        | _ -> usage ())
    | _ -> usage ()
  in
  (*n*)
  let lexbuf = Lexing.from_channel input_channel in
  let _ =
    if !interactive then
      Printf.printf "%!        Welcome to Spoutnik, version %s\n%!" version
  in
  let rho = ref Semantics.init_env in
  while true do
    try
      if !interactive then Printf.printf "> %!";
      let sentence = Parser.main Lexer.lex lexbuf in
      let () =
        if !debug then (
          Ast.print_topdef stdout sentence;
          Printf.fprintf stdout "\n%!")
      in

      let ty, new_env = Infer.type_topdef !Types.init_env sentence in
      let value = Semantics.eval_sentence sentence rho in

      let () =
        if !interactive || !debug then (
          if !debug then Printf.printf "\n%!";
          Semantics.printval stdout value;
          Printf.printf ": %!";
          Types.print stdout ty;
          Printf.printf "\n%!")
      in
      (* On met à jour l'environement global de typage. *)
      Types.init_env := new_env
    with
    | Lexer.Eoi ->
        if !interactive then Printf.printf "Bye.\n%!";
        exit 0
    | Failure msg -> Printf.printf "Error: %s\n\n" msg
    | Unify.TypeCycle (v_name, ty) ->
        Printf.printf "Error: variable %s appears in type %a\n\n" v_name
          Types.print ty
    | Unify.TypeConflict (ty1, ty2) ->
        Printf.printf "Error: types %a and %a are incompatible\n\n" Types.print
          ty1 Types.print ty2
    | Unify.UnitCycle (v_name, u) ->
        Printf.printf "Error: variable %s appears in unit %a\n\n" v_name
          Types.print_unit u
    | Unify.UnitConflict (u1, u2) ->
        Printf.printf "Error: units %a and %a are incompatible\n\n"
          Types.print_unit u1 Types.print_unit u2
    | Parsing.Parse_error ->
        let sp = Lexing.lexeme_start_p lexbuf in
        let ep = Lexing.lexeme_end_p lexbuf in
        Format.printf "File %S, line %i, characters %i-%i: Syntax error.\n"
          sp.Lexing.pos_fname sp.Lexing.pos_lnum
          (sp.Lexing.pos_cnum - sp.Lexing.pos_bol)
          (ep.Lexing.pos_cnum - sp.Lexing.pos_bol)
    | Lexer.LexError (sp, ep) ->
        Printf.printf "File %S, line %i, characters %i-%i: Lexical error.\n"
          sp.Lexing.pos_fname sp.Lexing.pos_lnum
          (sp.Lexing.pos_cnum - sp.Lexing.pos_bol)
          (ep.Lexing.pos_cnum - sp.Lexing.pos_bol)
    | Types.Error msg -> Printf.printf "Error: %s.\n" msg
  done
;;

if !Sys.interactive then () else main ()

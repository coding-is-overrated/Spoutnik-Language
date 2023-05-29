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

let main () =
  let input_channel =
    match Array.length Sys.argv with
    | 1 -> stdin
    | 2 -> (
        match Sys.argv.(1) with
        | "-" -> stdin
        | name -> (
            try open_in name
            with _ ->
              Printf.eprintf "Opening %s failed\n%!" name;
              exit 1))
    | _ -> usage ()
  in
  (*n*)
  let lexbuf = Lexing.from_channel input_channel in
  let _ = Printf.printf "        Welcome to Spoutnik, version %s\n%!" version in
  let rho = ref Semantics.init_env in
  while true do
    try
      Printf.printf "> %!";
      let sentence = Parser.main Lexer.lex lexbuf in
      Ast.print_topdef stdout sentence;
      Printf.fprintf stdout " :\n%!";
      (* Ajouter sémantique *)
      let _ =
        Semantics.printval stdout (Semantics.eval_sentence sentence rho)
      in
      Printf.printf "\n%!";
      let ty, new_env = Infer.type_topdef !Types.init_env sentence in
      Types.print stdout ty;
      (* On met à jour l'environement global de typage. *)
      Types.init_env := new_env;
      Printf.printf "\n%!"
    with
    | Lexer.Eoi ->
        Printf.printf "Bye.\n%!";
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

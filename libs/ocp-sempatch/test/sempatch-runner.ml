open Std_utils

let test_progs = [
  "x", [ "simpleVar", "y" ];
  "f x y", [ "apply", "foo y"];
  "x", [ "patch1", "((+) x) 1" ];
  "fun x -> x", [ "patch1", "fun x  -> ((+) x) 1"; "functionMatch", "foo"];
  "let x = 1 in x", [ "letBinding", "tralala"; "replaceInsideLet", "let x = 1 in y"];
  "1, 2, 3, 4", [ "tuples", "foo"];
  "if List.length mylist = 0 then foo else bar", [ "listCompare", "match mylist with | [] -> foo | _ -> bar" ];
  "function | foo -> true | bar -> true", [ "function", "function | foo -> true | bar -> false"];
  "match x with foo -> true | bar -> false", [ "match", "x = foo" ];
  "match x with Some y -> true | None -> false", [ "matchPattern", "Option.is_some x" ]
]

let in_file = open_in Sys.argv.(1)

let patches = Sempatch.from_channel in_file

let string_to_expr s = Parser.parse_expression Lexer.token (Lexing.from_string s)
let expr_to_string e =
  Pprintast.expression Format.str_formatter e;
  Format.flush_str_formatter ()

let dump_env patch_name (env, _) =
  Printf.eprintf "==========\n%s : \n" patch_name;
      StringMap.iter (fun key value ->
          let dump =
            match value with
            | Variables.Expression e ->
              Pprintast.expression Format.str_formatter (Ast_helper.Exp.mk e);
              Format.flush_str_formatter ();
            | Variables.Ident i -> i
          in
          Printf.eprintf "[%s=%s]" key dump
        )
        env;
      Printf.eprintf "\n"

let test patches (ast, expected_results) =
  let parsed_ast = string_to_expr ast in
  StringMap.fold (fun patch_name patch accu ->
    List.map (fun (name, expected) ->
        if (name = patch_name) then
          let result = expr_to_string (Sempatch.apply patch parsed_ast) in
          List.iter (dump_env name) (Sempatch.get_matches_from_patch patch parsed_ast);
          Option.some_if (expected <> result) (name, result)
        else None
        )
    expected_results
    :: accu
  )
  patches
  []
  |> List.flatten

let () =
  let failure = ref false in
  List.map (test patches) test_progs
  |> List.iteri
       (fun i -> List.iter
          (fun ast_opt -> match ast_opt with
            | Some (name, ast) ->
              Printf.printf "Error applying patch %s at test %d : got " name i;
              failure := true;
              print_endline ast
            | None -> ()
          )
       );
  close_in in_file;
  if !failure
  then
    exit 1
  else
    exit 0

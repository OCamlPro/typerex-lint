open Std_utils

module H = Ast_helper

(**************************************************)
(* Id of type                                     *)
(**************************************************)

let id_of_path path =
  Path.name path
  |> String.map (fun c -> if c = '.' then '_' else c)
  |> String.uncapitalize

let rec id_of_typ_expr texpr =
  let open Types in
  match texpr.desc with
  | Ttuple exprs ->
    print_texpr_list exprs
  | Tconstr (path, [], _) ->
    Some (id_of_path path)
  | Tconstr (path, args, _) ->
    Option.map
      (fun args_str ->  args_str ^ "_" ^ (id_of_path path))
      (print_texpr_list args)
  | _ -> None

and print_texpr_list tlist =
  List.map id_of_typ_expr tlist
  |> List.flip_opt
  |> Option.map (String.concat "_")

(**************************************************)
(* Failure handling                               *)
(**************************************************)

let fail msg = Printf.ksprintf failwith msg

(**************************************************)
(* Conversion between Types.t and Parsetree types *)
(**************************************************)

let rec longident_of_path =
  let module L = Longident in
  let module P = Path in
  function
  | P.Pident i -> L.Lident (Ident.name i)
  | P.Pdot (path, name, _) -> L.Ldot (longident_of_path path, name)
  | P.Papply (lhs, rhs) ->
    L.Lapply (longident_of_path lhs, longident_of_path rhs)


let rec core_type_of_type_expr texpr =
  let open Types in
  match texpr.desc with
  | Ttuple args ->
    H.Typ.tuple (List.map core_type_of_type_expr args)
  | Tconstr (name, args, _) ->
    H.Typ.constr
      (Location.mknoloc @@ longident_of_path name)
      (List.map core_type_of_type_expr args)
  | _ ->
    fail "This type can't be converted to core_type"

(**************************************************)
(* Construction of longidents                     *)
(**************************************************)

module Pfx =
struct
  let mk prefix name =
    Longident.Ldot (Longident.Lident prefix, name)

  let cstr ident =
    match Longident.last ident with
    | "::" | "[]" as name -> Longident.Lident name
    | "Cons" -> Longident.Lident "::"
    | "Nil" -> Longident.Lident "[]"
    | _ -> begin
        match ident with
        | Longident.Lident name ->
          Longident.Lident (String.capitalize name)
        | Longident.Ldot (id, name) ->
          Longident.Ldot (id, String.capitalize name)
        | Longident.Lapply _ -> assert false
      end

  let t = mk "T"
  let st = mk "St"
  let n = mk "Nodes"
end

let (!!) s = s ^ "_"

let flatten li =
  Longident.flatten li
  |> List.map String.uncapitalize
  |> String.concat "_"
  |> (fun s -> Longident.Lident s)

(**************************************************)
(* Generation of arguments                        *)
(**************************************************)

let nth_arg n = "arg_" ^ (string_of_int n)
let nth_state n = "state_" ^ (string_of_int n)

let gen_args lst = List.mapi (fun i _ -> nth_arg i) lst
let gen_states lst = List.mapi (fun i _ -> nth_state i) lst

(**************************************************)
(* Searching in type environment                  *)
(**************************************************)

let get_type name env =
  try
    Some (List.assoc name env)
  with
    Not_found -> None


let deriving = [Location.mknoloc "deriving", Parsetree.PStr [%str ord, show]]

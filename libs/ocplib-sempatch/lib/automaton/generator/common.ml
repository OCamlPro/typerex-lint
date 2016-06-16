module LI = Longident
module L =Location
open Parsetree
open Asttypes
open Std_utils

let print_longident lid = String.concat "__" (LI.flatten lid)

let tuple_constr loc args =
  let constr =
    LI.Lident ("tuple_" ^ (string_of_int (List.length args)))
  in
  Ptyp_constr (L.mkloc constr loc, args)

let rec upprint typ =
  let open Option.Infix in
  match typ.ptyp_desc with
  | Ptyp_constr (id, args) ->
    (Some (print_longident id.txt)) :: List.map upprint args
    |> List.flip_opt
    >|= (String.concat "_")
  | Ptyp_tuple args ->
    upprint {typ with ptyp_desc = tuple_constr typ.ptyp_loc args; }
  | _ -> None

let type_of_string str =
  Parser.parse_core_type Lexer.token (Lexing.from_string str)

let id = String.uncapitalize
let cstr str =
  match String.capitalize str with
  | "Cons" -> "::"
  | "Nil" -> "[]"
  | s -> s

let instantiate_type_decl variables_def typ =
  let mapper = let open Ast_mapper in {
      default_mapper with
      typ = (fun self typ ->
          let mapped =
            match typ.ptyp_desc with
            | Ptyp_var v ->
              begin
                try
                  List.assoc v variables_def
                with
                  Not_found -> typ
              end
            | _ -> typ
          in default_mapper.typ self mapped
        );
    }
    in mapper.Ast_mapper.type_declaration mapper typ

let eprint prefix arg =
  let () = Printf.eprintf "%s : " prefix in
  Printf.eprintf arg
let warn msg = eprint "Warning" msg
let debug msg = eprint "Debug" msg

let raise_errorf ?loc msg =
  Printf.ksprintf
    (fun s -> raise
      (Location.Error (Location.error ?loc s))
    )
    msg

let get_val_decls = List.bind (
    fun stri -> match stri.pstr_desc with
      | Pstr_value (_, t) -> t
      | _ -> []
  )

let get_type_decls = List.bind (
    fun stri -> match stri.pstr_desc with
      | Pstr_type t -> t
      | _ -> []
  )

let stdlib =
  let str = [%str
    type 'a list =
      | Nil
      | Cons of 'a * 'a list
    type 'a option =
      | None
      | Some of 'a
    type unit
    type bool
    type int
    type char
    type string
    type int32
    type int64
    type nativeint
    type lexing__position = Lexing.position = {
      pos_fname : string;
      pos_lnum : int;
      pos_bol : int;
      pos_cnum : int;
    }
    type location__t = Location.t = {
      loc_start: Lexing.position;
      loc_end: Lexing.position;
      loc_ghost: bool;
    }
    type longident__t = Longident.t =
        Lident of string
      | Ldot of Longident.t * string
      | Lapply of Longident.t * Longident.t

    type constant =
        Const_int of int
      | Const_char of char
      | Const_string of string * string option
      | Const_float of string
      | Const_int32 of int32
      | Const_int64 of int64
      | Const_nativeint of nativeint

    and rec_flag = Nonrecursive | Recursive
    and direction_flag  = Upto | Downto
    and private_flag = Private | Public
    and mutable_flag = Immutable | Mutable
    and virtual_flag = Virtual | Concrete
    and override_flag = Override | Fresh
    and closed_flag = Closed | Open
    and label
    and 'a loc = {
      txt : 'a;
      loc : Location.t;
    }

    and variance =
      | Covariant
      | Contravariant
      | Invariant
    ]
in get_type_decls str

(* [filter_decls type_decls] returns the list of monomorphic type
declarations in type_decls *)
let filter_decls = List.filter (fun decl -> decl.ptype_params = [])

let concrete_stdlib = filter_decls stdlib

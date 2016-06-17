(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                        Alain Frisch, LexiFi                         *)
(*                                                                     *)
(*  Copyright 2012 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* A generic Parsetree mapping class *)

(*
[@@@ocaml.warning "+9"]
  (* Ensure that record patterns don't miss any field. *)
*)


open Asttypes
open Parsetree
open Ast_helper
open Location

type ppx = {
  structure : structure -> structure;
  signature : signature -> signature;
}

let rec extension_of_error {loc; msg; if_highlight; sub} =
  { loc; txt = "ocaml.error" },
  PStr ([Str.eval (Exp.constant (Const_string (msg, None)));
         Str.eval (Exp.constant (Const_string (if_highlight, None)))] @
        (List.map (fun ext -> Str.extension (extension_of_error ext)) sub))

let attribute_of_warning loc s =
  { loc; txt = "ocaml.ppwarning" },
  PStr ([Str.eval ~loc (Exp.constant (Const_string (s, None)))])

module StringMap = Map.Make(struct
    type t = string
    let compare = compare
end)

let cookies = ref StringMap.empty

let get_cookie k =
  try Some (StringMap.find k !cookies)
  with Not_found -> None

let set_cookie k v =
  cookies := StringMap.add k v !cookies

let tool_name_ref = ref "_none_"

let tool_name () = !tool_name_ref


module PpxContext = struct
  open Longident
  open Asttypes
  open Ast_helper

  let lid name = { txt = Lident name; loc = Location.none }

  let make_string x = Exp.constant (Const_string (x, None))

  let make_bool x =
    if x
    then Exp.construct (lid "true") None
    else Exp.construct (lid "false") None

  let rec make_list f lst =
    match lst with
    | x :: rest ->
      Exp.construct (lid "::") (Some (Exp.tuple [f x; make_list f rest]))
    | [] ->
      Exp.construct (lid "[]") None

  let make_pair f1 f2 (x1, x2) =
    Exp.tuple [f1 x1; f2 x2]

  let make_option f opt =
    match opt with
    | Some x -> Exp.construct (lid "Some") (Some (f x))
    | None   -> Exp.construct (lid "None") None

  let get_cookies () =
    lid "cookies",
    make_list (make_pair make_string (fun x -> x))
      (StringMap.bindings !cookies)

  let mk fields =
    { txt = "ocaml.ppx.context"; loc = Location.none },
    Parsetree.PStr [Str.eval (Exp.record fields None)]

  let make ~tool_name () =
    let fields =
      [
        lid "tool_name",    make_string tool_name;
        lid "include_dirs", make_list make_string !Clflags.include_dirs;
        lid "load_path",    make_list make_string !Config.load_path;
        lid "open_modules", make_list make_string !Clflags.open_modules;
        lid "for_package",  make_option make_string !Clflags.for_package;
        lid "debug",        make_bool !Clflags.debug;
        get_cookies ()
      ]
    in
    mk fields

  let get_fields = function
    | PStr [{pstr_desc = Pstr_eval
                 ({ pexp_desc = Pexp_record (fields, None) }, [])}] ->
        fields
    | _ ->
        raise_errorf "Internal error: invalid [@@@ocaml.ppx.context] syntax"

  let restore fields =
    let field name payload =
      let rec get_string = function
        | { pexp_desc = Pexp_constant (Const_string (str, None)) } -> str
        | _ ->
            raise_errorf
              "Internal error: invalid [@@@ocaml.ppx.context { %s }] string syntax"
              name
      and get_bool pexp =
        match pexp with
        | {pexp_desc = Pexp_construct ({txt = Longident.Lident "true"}, None)} ->
            true
        | {pexp_desc = Pexp_construct ({txt = Longident.Lident "false"}, None)} ->
            false
        | _ ->
            raise_errorf
              "Internal error: invalid [@@@ocaml.ppx.context { %s }] bool syntax"
              name
      and get_list elem = function
        | {pexp_desc =
             Pexp_construct ({txt = Longident.Lident "::"},
                             Some {pexp_desc = Pexp_tuple [exp; rest]}) } ->
            elem exp :: get_list elem rest
        | {pexp_desc =
             Pexp_construct ({txt = Longident.Lident "[]"}, None)} ->
            []
        | _ ->
            raise_errorf
              "Internal error: invalid [@@@ocaml.ppx.context { %s }] list syntax"
              name
      and get_pair f1 f2 = function
        | {pexp_desc = Pexp_tuple [e1; e2]} ->
            (f1 e1, f2 e2)
        | _ ->
            raise_errorf
              "Internal error: invalid [@@@ocaml.ppx.context { %s }] pair syntax"
              name
      and get_option elem = function
        | { pexp_desc =
              Pexp_construct ({ txt = Longident.Lident "Some" }, Some exp) } ->
            Some (elem exp)
        | { pexp_desc =
              Pexp_construct ({ txt = Longident.Lident "None" }, None) } ->
            None
        | _ ->
            raise_errorf
              "Internal error: invalid [@@@ocaml.ppx.context { %s }] option syntax"
              name
      in
      match name with
      | "tool_name" ->
          tool_name_ref := get_string payload
      | "include_dirs" ->
          Clflags.include_dirs := get_list get_string payload
      | "load_path" ->
          Config.load_path := get_list get_string payload
      | "open_modules" ->
          Clflags.open_modules := get_list get_string payload
      | "for_package" ->
          Clflags.for_package := get_option get_string payload
      | "debug" ->
          Clflags.debug := get_bool payload
      | "cookies" ->
          let l = get_list (get_pair get_string (fun x -> x)) payload in
          cookies :=
            List.fold_left
              (fun s (k, v) -> StringMap.add k v s) StringMap.empty
              l
      | _ ->
          ()
    in
    List.iter (function ({txt=Lident name}, x) -> field name x | _ -> ()) fields

  let update_cookies fields =
    let fields =
      List.filter
        (function ({txt=Lident "cookies"}, _) -> false | _ -> true)
        fields
    in
    fields @ [get_cookies ()]
end

let ppx_context = PpxContext.make


let apply_lazy ~source ~target ppx =
  let ic = open_in_bin source in
  let magic =
    really_input_string ic (String.length Config.ast_impl_magic_number)
  in
  if magic <> Config.ast_impl_magic_number
  && magic <> Config.ast_intf_magic_number then
    failwith "Ast_ppx: OCaml version mismatch or malformed input";
  Location.input_name := input_value ic;
  let ast = input_value ic in
  close_in ic;

  let implem ast =
    try
      let fields, ast =
        match ast with
        | {pstr_desc = Pstr_attribute ({txt = "ocaml.ppx.context"}, x)} :: l ->
            PpxContext.get_fields x, l
        | _ -> [], ast
      in
      PpxContext.restore fields;
      let ppx = ppx () in
      let ast = ppx.structure ast in
      let fields = PpxContext.update_cookies fields in
      Str.attribute (PpxContext.mk fields) :: ast
    with exn ->
      match error_of_exn exn with
      | Some error ->
          [{pstr_desc = Pstr_extension (extension_of_error error, []);
            pstr_loc  = Location.none}]
      | None -> raise exn
  in
  let iface ast =
    try
      let fields, ast =
        match ast with
        | {psig_desc = Psig_attribute ({txt = "ocaml.ppx.context"}, x)} :: l ->
            PpxContext.get_fields x, l
        | _ -> [], ast
      in
      PpxContext.restore fields;
      let ppx = ppx () in
      let ast = ppx.signature ast in
      let fields = PpxContext.update_cookies fields in
      Sig.attribute (PpxContext.mk fields) :: ast
    with exn ->
      match error_of_exn exn with
      | Some error ->
          [{psig_desc = Psig_extension (extension_of_error error, []);
            psig_loc  = Location.none}]
      | None -> raise exn
  in
  let ast =
    if magic = Config.ast_impl_magic_number
    then Obj.magic (implem (Obj.magic ast))
    else Obj.magic (iface (Obj.magic ast))
  in
  let oc = open_out_bin target in
  output_string oc magic;
  output_value oc !Location.input_name;
  output_value oc ast;
  close_out oc

let drop_ppx_context_str ~restore = function
  | {pstr_desc = Pstr_attribute({Location.txt = "ocaml.ppx.context"}, a)}
    :: items ->
      if restore then
        PpxContext.restore (PpxContext.get_fields a);
      items
  | items -> items

let drop_ppx_context_sig ~restore = function
  | {psig_desc = Psig_attribute({Location.txt = "ocaml.ppx.context"}, a)}
    :: items ->
      if restore then
        PpxContext.restore (PpxContext.get_fields a);
      items
  | items -> items

let add_ppx_context_str ~tool_name ast =
  Ast_helper.Str.attribute (ppx_context ~tool_name ()) :: ast

let add_ppx_context_sig ~tool_name ast =
  Ast_helper.Sig.attribute (ppx_context ~tool_name ()) :: ast


let apply ~source ~target ppx =
  apply_lazy ~source ~target (fun () -> ppx)

let run_main ppx =
  try
    let a = Sys.argv in
    let n = Array.length a in
    if n > 2 then
      let ppx () =
        try ppx (Array.to_list (Array.sub a 1 (n - 3)))
        with exn ->
          (* PR #6463 *)
          let f _ = raise exn in
          {structure = f; signature = f}
      in
      apply_lazy ~source:a.(n - 2) ~target:a.(n - 1) ppx
    else begin
      Printf.eprintf "Usage: %s [extra_args] <infile> <outfile>\n%!"
                     Sys.executable_name;
      exit 2
    end
  with exn ->
    prerr_endline (Printexc.to_string exn);
    exit 2

let register_function = ref (fun _name f -> run_main f)
let register name f = !register_function name f

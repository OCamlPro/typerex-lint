open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree

type t = mapper list

let mkppx patch = {
  default_mapper with
  structure = (
    fun _ structure ->
      List.fold_left (fun iter elt -> elt.structure elt iter) structure (List.rev patch)
  );

  signature = (
    fun _ signature ->
      List.fold_left (fun iter elt -> elt.signature elt iter) signature (List.rev patch)
  );
}

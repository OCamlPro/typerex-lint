open Ast_mapper

type t = mapper

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

let register name m = Ast_mapper.register name (fun _ -> mkppx m)

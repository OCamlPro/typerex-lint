module Ast_element:
sig
  type t = Ast_element.t

  val to_string : t -> string

  val from_structure : Parsetree.structure -> t
  val from_expression : Parsetree.expression -> t
end

module Substitution:
sig
  type t

  val get : string -> t -> Ast_element.t option

  val to_list : t -> (string * Ast_element.t) list
end

module Match:
sig
  type t (* = Environment.t *)

  (** Return the name of the patch who matched *)
  val get_patch_name : t -> string

  (** Return the location of the match *)
  val get_location : t -> Location.t

  (** Return the stubstitution of all the free variables in the patch *)
  val get_substitutions : t -> Substitution.t

  (** Return the matched ast element after application of the patch *)
  (* val get_patched_tree : t -> Ast_element.t *) (* When implemented *)
end

module Patch:
sig
  type t (* = Parsed_patches.Typ.t *)

  (** Generate a list of patches *)
  val from_channel : in_channel -> t list

  (** Accessors for patch metadatas *)
  val get_name : t -> string
  val get_msg : t -> string option
  val get_metavariables : t -> string list
  val get_field : string -> t -> string option

  (** {2 Application of patches} *)

  (** [apply patch tree] applyes [patch] to [tree]
      and returns the list of matches *)
  val apply : t -> Ast_element.t -> Match.t list

  (** [parallel_apply patches tree] applies all the patches to [tree] and
      returns the concatenation of all the matches
  *)
  val parallel_apply : t list -> Ast_element.t -> Match.t list
end

module Failure:
sig
  type t

  exception SempatchException of t
  val to_string : t -> string
end

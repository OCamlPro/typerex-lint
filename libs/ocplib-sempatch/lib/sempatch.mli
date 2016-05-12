module Ast_element:
sig
  type t = Ast_element.t =
    | Expression of Parsetree.expression
    | Ident of string

  val to_string : t -> string
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
  (* val get_field : t -> string -> string *) (* When implemented *)

  (** {2 Application of patches} *)

  (** [apply patch tree] applyes [patch] to [tree]
      and returns the couple [patched_tree, matches] *)
  val apply : t -> Ast_element.t -> Ast_element.t * Match.t list

  (** Same as [apply] except that it tries to match patches only at the root of the AST *)
  val apply_nonrec : t -> Ast_element.t -> Ast_element.t * Match.t list

  (** [sequential_apply patches tree] applies applies all the patches in order
      to tree ({i ie} the first patch [p1] is applied to [tree], the second one
      to the result [tree'] of the application of [p1] to [tree], and so on.
      (So beware that the matches may match expressions who are the result of
      previous patches and aren't in the original AST)
  *)
  val sequential_apply : t list -> Ast_element.t -> Ast_element.t * Match.t list

  (** [parallel_apply patches tree] applies all the patches to [tree] and
      returns the concatenation of all the matches
  *)
  val parallel_apply : t list -> Ast_element.t -> Match.t list

  (** Same as [parallel_apply] except that it tries to match patches only at the root of the AST *)
  val parallel_apply_nonrec : t list -> Ast_element.t -> Match.t list
end

module Failure:
sig
  type t

  exception SempatchException of t
  val to_string : t -> string
end

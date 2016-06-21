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

(** The interface of a -ppx rewriter

  A -ppx rewriter is a program that accepts a serialized abstract syntax
  tree and outputs another, possibly modified, abstract syntax tree.
  This module encapsulates the interface between the compiler and
  the -ppx rewriters, handling such details as the serialization format,
  forwarding of command-line flags, and storing state.

  {[
open Asttypes
open Parsetree
open Ast_mapper

let test_mapper argv =
  { structure = (fun str -> ... );
    signature = (fun sg -> ...);
  }

let () =
  register "ppx_test" test_mapper]}

  This -ppx rewriter can be compiled using
  [ocamlc -o ppx_test -I +compiler-libs ocamlcommon.cma ppx_test.ml].

  *)

open Parsetree

type ppx = {
  structure : structure -> structure;
  signature : signature -> signature;
}

(** {2 A generic Parsetree mapper} *)

(** {2 Apply ppxs to compilation units} *)

val tool_name: unit -> string
(** Can be used within a ppx preprocessor to know which tool is
    calling it ["ocamlc"], ["ocamlopt"], ["ocamldoc"], ["ocamldep"],
    ["ocaml"], ...  Some global variables that reflect command-line
    options are automatically synchronized between the calling tool
    and the ppx preprocessor: [Clflags.include_dirs],
    [Config.load_path], [Clflags.open_modules], [Clflags.for_package],
    [Clflags.debug]. *)


val apply: source:string -> target:string -> ppx -> unit
(** Apply a ppx (parametrized by the unit name) to a dumped
    parsetree found in the [source] file and put the result in the
    [target] file. The [structure] or [signature] field of the ppx
    is applied to the implementation or interface.  *)

val run_main: (string list -> ppx) -> unit
(** Entry point to call to implement a standalone -ppx rewriter from a
    ppx, parametrized by the command line arguments.  The current
    unit name can be obtained from [Location.input_name].  This
    function implements proper error reporting for uncaught
    exceptions. *)

(** {2 Registration API} *)

val register_function: (string -> (string list -> ppx) -> unit) ref

val register: string -> (string list -> ppx) -> unit
(** Apply the [register_function].  The default behavior is to run the
    ppx immediately, taking arguments from the process command
    line.  This is to support a scenario where a ppx is linked as a
    stand-alone executable.

    It is possible to overwrite the [register_function] to define
    "-ppx drivers", which combine several ppxs in a single process.
    Typically, a driver starts by defining [register_function] to a
    custom implementation, then lets ppx rewriters (linked statically
    or dynamically) register themselves, and then run all or some of
    them.  It is also possible to have -ppx drivers apply rewriters to
    only specific parts of an AST.

    The first argument to [register] is a symbolic name to be used by
    the ppx driver.  *)


(** {2 Convenience functions to write ppxs} *)

val extension_of_error: Location.error -> extension
(** Encode an error into an 'ocaml.error' extension node which can be
    inserted in a generated Parsetree.  The compiler will be
    responsible for reporting the error. *)

val attribute_of_warning: Location.t -> string -> attribute
(** Encode a warning message into an 'ocaml.ppwarning' attribute which can be
    inserted in a generated Parsetree.  The compiler will be
    responsible for reporting the warning. *)

(** {2 Helper functions to call external ppxs} *)

val add_ppx_context_str: tool_name:string -> Parsetree.structure -> Parsetree.structure
(** Extract information from the current environment and encode it
    into an attribute which is prepended to the list of structure
    items in order to pass the information to an external
    processor. *)

val add_ppx_context_sig: tool_name:string -> Parsetree.signature -> Parsetree.signature
(** Same as [add_ppx_context_str], but for signatures. *)

val drop_ppx_context_str: restore:bool -> Parsetree.structure -> Parsetree.structure
(** Drop the ocaml.ppx.context attribute from a structure.  If
    [restore] is true, also restore the associated data in the current
    process. *)

val drop_ppx_context_sig: restore:bool -> Parsetree.signature -> Parsetree.signature
(** Same as [drop_ppx_context_str], but for signatures. *)

(** {2 Cookies} *)

(** Cookies are used to pass information from a ppx processor to
    a further invocation of itself, when called from the OCaml
    toplevel (or other tools that support cookies). *)

val set_cookie: string -> Parsetree.expression -> unit
val get_cookie: string -> Parsetree.expression option

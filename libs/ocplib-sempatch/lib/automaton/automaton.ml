open Location
open! Asttypes
open Parsetree
open Lexing
open Longident

open Ast_element

let () = let open! Match in ()

[%%create_automaton]
[%%create_match]

let final () = A.{
    final = true;
    transitions = [
      false, (fun env _ -> [Final, env])
    ];
  }

let trash () = A.{
    final = false;
    transitions = [
      false, (fun env _ -> [Trash, env])
    ];
  }

module Match :
sig
  include module type of Match_
  val wildcard : unit -> A.state

  val metavar_expr : string -> A.state
  val metavar_pat : string -> A.state
end =
struct
  include Match_
  module E = Element

  let location__t _ _ _ = basic_state @@ function
    | E.Location__t _ -> [A.Final]
    | _ -> [A.Trash]

  let metavar_pat name = A.{
      final = false;
      transitions = [
        false,
        fun meta ast_elt ->
          match ast_elt with
          | E.Pattern pat ->
            [
              Final,
              {
                meta with
                Match.substitutions =
                  Substitution.add_pattern
                    name
                    pat
                    meta.Match.substitutions;
              }
            ]
          | _ -> []
      ]
    }

  let metavar_expr name = A.{
      final = false;
      transitions = [
        false,
        fun meta ast_elt ->
          match ast_elt with
          | E.Expression expr ->
            [
              Final,
              {
                meta with
                Match.substitutions =
                  Substitution.add_expr
                    name
                    expr
                    meta.Match.substitutions;
              }
            ]
          | _ -> []
      ]
    }

  [%%create_wildcard]
end

let make_report state =
  {
    state with
    A.transitions = List.map (
        fun (_, f) -> true, f
      )
        state.A.transitions
  }

let add_transitions_from dest origin =
  dest.A.transitions <- origin.A.transitions @ dest.A.transitions;
  dest

let has_attr name attributes =
  List.exists
    (fun (id, _) -> id.Asttypes.txt = name)
    attributes

[%%create_from]

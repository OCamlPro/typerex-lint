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
  end =
struct
  include Match_
  module E = Element

  let location__t _ _ _ = basic_state @@ function
    | E.Location__t _ -> [A.Final]
    | _ -> [A.Trash]

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

[%%create_from]

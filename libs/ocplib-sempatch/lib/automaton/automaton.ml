open Location
open! Asttypes
open Parsetree
open Lexing
open Longident

open Ast_element

let () = let open! Match in ()

[%%create_automaton]
[%%create_match]

module Match : module type of Match_ =
struct
  include Match_
  module E = Element

  let location__t _ _ _ = basic_state @@ function
    | E.Location__t _ -> [A.Final]
    | _ -> [A.Trash]

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

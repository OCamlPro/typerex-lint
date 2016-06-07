open Location
open! Asttypes
open Parsetree
open Lexing
open Longident

open Ast_element

[%%create_automaton]
[%%create_match]

module Match : module type of Match_ =
struct
  include Match_

  let location__t _ _ _ = basic_state @@ function
    | Element.Location__t _ -> [A.Final]
    | _ -> [A.Trash]
end

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

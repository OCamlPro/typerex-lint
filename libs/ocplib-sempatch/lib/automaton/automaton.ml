open Location
open! Asttypes
open Parsetree
open Lexing
open Longident

open Match

open Ast_element

[%%create_automaton]
[%%create_match]
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

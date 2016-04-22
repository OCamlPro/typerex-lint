open SimpleConfig (* for !! *)

(* We will register this linter to the Mascot plugin. *)
module Mascot = Plugin_mascot.PluginMascot

let details = "Detects useless 'if'."

module CodeIdentifierLength = Mascot.MakeLint(struct
    let name = "Useless if"
    let short_name = "code-useless-if"
    let details = details
  end)

type warnings = UselessIf | ConstantIf

module Warnings = CodeIdentifierLength.MakeWarnings(struct
    type t = warnings

    let useless loc args = CodeIdentifierLength.new_warning
        loc
        1
        [ Warning.kind_code ]
        ~short_name:"useless-if"
        ~msg:"Useless if-then-else construction."
        ~args

    let constant loc args = CodeIdentifierLength.new_warning
        loc
        2
        [ Warning.kind_code ]
        ~short_name:"constant-if"
        ~msg:"Constant if-then-else construction."
        ~args

    let report loc = function
      | UselessIf -> useless loc []
      | ConstantIf -> constant loc []
  end)

let iter =
  let module IterArg = struct
    include ParsetreeIter.DefaultIteratorArgument

    let rec equal true_ false_ =
      let open Parsetree in
      let open Asttypes in
      begin match true_.pexp_desc, false_.pexp_desc with
        | Pexp_ident true_, Pexp_ident false_ -> true_.txt = false_.txt
        | Pexp_constant true_, Pexp_constant false_ ->
          true_ = false_
        | Pexp_apply (f1, args1), Pexp_apply (f2, args2) ->
          equal f1 f2 &&
          (List.for_all2
             (fun (lbl1, arg1) (lbl2, arg2) -> lbl1 = lbl2 && equal arg1 arg2)
             args1 args2)
        | e1, e2 -> e1 = e2 end

    let enter_expression expr =
      let open Parsetree in
      let open Asttypes in
      begin match expr.pexp_desc with
        | Pexp_ifthenelse (cond, then_expr, else_expr) ->
          begin match then_expr.pexp_desc, else_expr with
            | Pexp_construct (true_expr, _),
              Some ({pexp_desc = Pexp_construct (false_expr, _)}) ->
              if (Longident.last true_expr.txt = "true" &&
                  Longident.last false_expr.txt = "false") ||
                 (Longident.last true_expr.txt = "false" &&
                  Longident.last false_expr.txt = "true") then
                Warnings.report expr.pexp_loc UselessIf
            | _ -> ()
          end;
          begin match else_expr with
            | Some false_expr ->
              if equal then_expr false_expr then
                Warnings.report expr.pexp_loc ConstantIf
            | _ -> ()
          end;
        | _ -> ()
      end
  end in (* end IterArg *)
  (module IterArg : ParsetreeIter.IteratorArgument)

(* Registering a main entry to the linter *)
module MainML = CodeIdentifierLength.MakeInputStructure(struct
    let main ast = ParsetreeIter.iter_structure iter ast
  end)

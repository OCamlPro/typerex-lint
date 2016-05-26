(**************************************************************************)
(*                                                                        *)
(*                        OCamlPro Typerex                                *)
(*                                                                        *)
(*   Copyright OCamlPro 2011-2016. All rights reserved.                   *)
(*   This file is distributed under the terms of the GPL v3.0             *)
(*   (GNU General Public Licence version 3.0).                            *)
(*                                                                        *)
(*     Contact: <typerex@ocamlpro.com> (http://www.ocamlpro.com/)         *)
(*                                                                        *)
(*  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,       *)
(*  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES       *)
(*  OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND              *)
(*  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS   *)
(*  BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN    *)
(*  ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN     *)
(*  CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE      *)
(*  SOFTWARE.                                                             *)
(**************************************************************************)

open SimpleConfig (* for !! *)

(* We will register this linter to the Mascot plugin. *)
module Core = Plugin_core.PluginCore

let details = "Detects useless 'if'."

module CodeUseless = Core.MakeLint(struct
    let name = "Useless if"
    let short_name = "code-useless-if"
    let details = details
  end)

type warnings = UselessIf | ConstantIf

let useless = CodeUseless.new_warning
    [ Warning.kind_code ]
    ~short_name:"useless-if"
    ~msg:"Useless if-then-else construction."

let constant = CodeUseless.new_warning
    [ Warning.kind_code ]
    ~short_name:"constant-if"
    ~msg:"Constant if-then-else construction."

module Warnings = struct
  let useless = CodeUseless.instanciate useless
  let constant = CodeUseless.instanciate constant

  let report loc = function
    | UselessIf -> useless loc []
    | ConstantIf -> constant loc []
end

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
        (* | [% if cond then then_expr ] *)
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
module MainML = CodeUseless.MakeInputStructure(struct
    let main ast = ParsetreeIter.iter_structure iter ast
  end)

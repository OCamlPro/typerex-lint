(**************************************************************************)
(*                                                                        *)
(*                              OCamlPro TypeRex                          *)
(*                                                                        *)
(*   Copyright OCamlPro 2011-2016. All rights reserved.                   *)
(*   This file is distributed under the terms of the GPL v3.0             *)
(*      (GNU Public Licence version 3.0).                                 *)
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

open Asttypes
open Parsetree
open Ast_mapper
open Check_types
open Configuration
open Info
open Reports

let details =
  "Detects useless let binding.\n"

let info = {
  name = "Identity let";
  details;
  cat = Code;
}

let rec compare_pat_exp p e =
  match p.ppat_desc, e.pexp_desc with
  | Ppat_var { txt = pat_var }, Pexp_ident { txt = Longident.Lident exp_var } ->
    pat_var = exp_var
  | Ppat_tuple pat_list, Pexp_tuple exp_list ->
    begin try
      List.for_all2 (fun pat exp -> compare_pat_exp pat exp) pat_list exp_list
      with Invalid_argument _ -> false
    end
  | _, _-> false

let mapper config reports =
  { default_mapper with
    expr = fun mapper expr ->
      let msg =
        Printf.sprintf "Identity let detected. The let-binding is useless." in
        begin match expr.pexp_desc with
          | Pexp_let (flag, vbl, in_expr) ->
            let vbl =
              begin match vbl with
                | [] -> assert false
                | [vb] ->
                  if compare_pat_exp vb.pvb_pat in_expr then
                    Reports.add (Reports.warning expr.pexp_loc info msg) reports;
                  [vb]
                | vbl ->
                  List.iter (fun vb ->
                      if compare_pat_exp vb.pvb_pat in_expr then
                        Reports.add
                          (Reports.warning expr.pexp_loc info msg)
                          reports)
                    vbl;
                  List.map (default_mapper.value_binding mapper) vbl
              end in
            Ast_helper.Exp.let_ flag vbl (default_mapper.expr mapper in_expr)
          | _ -> default_mapper.expr mapper expr
        end
  }

let run config reports source =
  ignore (default_mapper.structure (mapper config reports) source)

let check : Check_types.source_check = { source_run = run; source_info = info }

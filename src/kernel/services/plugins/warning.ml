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

open Warning_types

let kind_code = Code
let kind_typo = Typo
let kind_interface = Interface
let kind_metrics = Metrics

let new_kind kind = Custom kind

let kind_to_string = function
  | Code -> "code"
  | Typo -> "typographie"
  | Interface -> "interface"
  | Metrics -> "metrics"
  | Custom kind -> kind

module Warning = struct

  (* Warning Set *)
  module WarningSet = Set.Make (struct
      type t = Warning_types.warning
      let compare = Pervasives.compare
    end)

  type t = WarningSet.t ref

  let empty () = ref WarningSet.empty

  let add_warning warning wset =
    wset := WarningSet.add warning !wset

  let add loc id decl output wset =
    let instance = {id; decl} in
    let warning = {loc; instance; output} in
    add_warning warning wset

  let length wset = WarningSet.cardinal !wset

  let iter apply wset = WarningSet.iter apply !wset

  let print ppf warning =
    if warning.loc <> Location.none then
      Format.fprintf ppf "%a" Location.print warning.loc;

    Format.fprintf ppf "  Warning %d: %s" warning.instance.id warning.output;
    Format.fprintf ppf "@."
end

module WarningDeclaration = struct
  (* Warning declaration Set *)
  module WDeclSet = Set.Make (struct
      type t = Warning_types.warning_declaration
      let compare = Pervasives.compare
    end)

  type t = WDeclSet.t ref

  let empty () = ref WDeclSet.empty

  let add decl decls = decls := WDeclSet.add decl !decls
end

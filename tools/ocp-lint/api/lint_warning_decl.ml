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

module WarningDeclaration = struct
  (* Warning declaration Set *)
  module WDeclSet = Set.Make (struct
      type t = Lint_warning_types.warning_declaration
      let compare w1 w2 =
        let id1 = w1.Lint_warning_types.id in
        let id2 = w2.Lint_warning_types.id in
        let id_diff = Pervasives.compare id1 id2 in
        if id_diff = 0 then Pervasives.compare w1 w2
        else id_diff
    end)

  type t = WDeclSet.t ref

  let empty () = ref WDeclSet.empty

  let add decl decls = decls := WDeclSet.add decl !decls

  let union decls1 decls2 = ref (WDeclSet.union !decls1 !decls2)

  let iter f decls = WDeclSet.iter f !decls

  let fold f decl acc = WDeclSet.fold f !decl acc
end

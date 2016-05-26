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

open SimpleConfig     (* for !! *)
open Plugin_file_system (* Registering to PluginFileSystem *)

let details = "Missing interface."

module MissingInterface = PluginFileSystem.MakeLint(struct
    let name = "Missing interface"
    let short_name = "interface_missing"
    let details = details
  end)

type warning = Missing of string

let missing = MissingInterface.new_warning
    [ Lint_warning.kind_interface ]
    ~short_name:"missing_interface"
    ~msg:"Missing interface for '$file'."

module Warnings = struct
  let missing = MissingInterface.instanciate missing

  let report loc = function
    | Missing file ->
      missing loc [("file", file)]
end

let mli = ".mli"

let check source =
  let modname = Filename.chop_extension source in
  let loc = Location.in_file source in
  if not (Sys.file_exists (modname ^ mli)) then
    Warnings.report loc (Missing source)

(* Registering a main entry to the linter *)
module MainSRC = MissingInterface.MakeInputML(struct
    let main source = check source
  end)

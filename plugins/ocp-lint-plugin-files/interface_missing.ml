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

open Plugin_file_system (* Registering to PluginFileSystem *)

let details = "Missing interface."

module MissingInterface = PluginFileSystem.MakeLint(struct
    let name = "Missing interface"
    let short_name = "interface_missing"
    let details = details
    let enable = false
  end)

type warning = Missing of string

let missing = MissingInterface.new_warning
    ~id:1
    ~short_name:"missing_interface"
    ~msg:"Missing interface for '$file'."

module Warnings = MissingInterface.MakeWarnings(struct
    type t = warning

    let to_warning = function
      | Missing file ->
        missing, [("file", file)]
  end)

let mli = ".mli"

let check source =
  let modname = Filename.chop_extension source in
  if not (Sys.file_exists (modname ^ mli)) then
    Warnings.report_file source (Missing source)

(* Registering a main entry to the linter *)
module MainSRC = MissingInterface.MakeInputML(struct
    let main source = check source
  end)

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

open StringCompat

open Plugin_file_system (* Registering to PluginFileSystem *)

let details = "Check Project File Names"

module Linter = PluginFileSystem.MakeLint(struct
    let name = "File Names"
    let version = "1"
    let short_name = "project_files"
    let details = details
    let enabled = true
  end)

type warning =
  | ConflictingSources of string * string
  | CapitalizedFilename of string

let conflicting = Linter.new_warning
    ~id:1
    ~short_name:"conflicting_sources"
    ~msg:"Source files $fileA and $fileB may conflict on some file-systems"
    ~severity:6

let capitalized = Linter.new_warning
    ~id:2
    ~short_name:"capitalized_filename"
    ~msg:"Filename $file should not be capitalized"
    ~severity:6

module Warnings = Linter.MakeWarnings(struct
    type t = warning

    let to_warning = function
      | ConflictingSources (fileA, fileB) ->
        conflicting, ["fileA", fileA; "fileB", fileB]
      | CapitalizedFilename filename ->
        capitalized, ["file", filename]
  end)

let check_files all =
  let files = ref StringMap.empty in
  List.iter (fun filename ->
      let basename = Filename.basename filename in
      let dirname = Filename.dirname filename in
      let lower_basename = String.lowercase basename in
      if Filename.check_suffix lower_basename ".ml"
      || Filename.check_suffix lower_basename ".mli"
      || Filename.check_suffix lower_basename ".mll"
      || Filename.check_suffix lower_basename ".mly"
      then begin

        begin
          let key = Filename.concat dirname lower_basename in
          try
            let fileA = StringMap.find key !files in
            let loc = Location.in_file filename in
            Warnings.report loc (ConflictingSources(fileA,filename))
          with Not_found ->
            files := StringMap.add key filename !files
        end;

        begin
          match basename.[0] with
          | 'A'..'Z' ->
            let loc = Location.in_file filename in
            Warnings.report loc (CapitalizedFilename filename)
          | _ -> ()
        end
      end
    ) all

(* Registering a main entry to the linter *)
module Main = Linter.MakeInputAll(struct
    let main all = check_files all
  end)

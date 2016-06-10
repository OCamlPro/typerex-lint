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


let iter_files ?(recdir=true) apply dirname =
  let rec iter dirname dir =
    let files = Sys.readdir (Filename.concat dirname dir) in
    Array.iter (fun file ->
        try
          let file = Filename.concat dir file in
          if Sys.is_directory (Filename.concat dirname file) then begin
            if recdir then iter dirname file
          end else
            apply file
        with Sys_error err ->
          Printf.eprintf "Scanning Error: %s\n%!" err)
      files
  in
  iter dirname ""

let substitute str substs =
  let replace substs str = try List.assoc str substs with Not_found -> str in
  let buf = Buffer.create (String.length str) in
  Buffer.add_substitute buf (replace substs) str;
  Buffer.contents buf

let absolute_path file =
  let file_t = File.of_string file in
  if File.is_absolute file_t then (File.to_string file_t)
  else
    let file = File.concat (File.getcwd ()) file_t in
    File.to_string file

let relative_path =
  let split_path = Str.split (Str.regexp (Filename.dir_sep)) in
  let rec make_relative = function
    | (dir1::root, dir2::file) when dir1 = dir2 -> make_relative (root, file)
    | (root, file) ->
        List.fold_left (fun path _ -> Filename.parent_dir_name::path) file root
  in
  fun root file ->
    make_relative (split_path root, split_path file)
      |> String.concat Filename.dir_sep


let find_root root_dir basenames =
  let rec find dirname (basenames : string list) =
    let file = File.add_basenames dirname basenames in
    if File.X.exists file then dirname else
      let new_dirname = File.dirname dirname in
      if new_dirname == dirname then raise Not_found;
      find new_dirname basenames
  in
  let root_dir = if File.is_absolute root_dir then root_dir else
      File.concat (File.X.getcwd ()) root_dir
  in
  find root_dir basenames

let is_in_path file path =
  Str.string_match (Str.regexp path) file 0

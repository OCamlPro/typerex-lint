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

open OcpCompat

type file_struct = {
  name : string;
  norm : string;
  hash : string;
  cmt : string option;
  ignored : string list;
}

let iter_files ?(recdir=true) apply dirname =
  let rec iter dirname dir =
    let files = Sys.readdir (Filename.concat dirname dir) in
    Array.iter (fun file ->
        try
          let file_p = Filename.concat dir file in
          if Sys.file_exists (Filename.concat dirname file_p) then
            if Sys.is_directory (Filename.concat dirname file_p) then begin
              if not (String.get file 0 = '.') &&
                 not (String.get file 0 = '_') &&
                 recdir then
                iter dirname file_p
            end else
              apply file_p
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

(* Beware: not Windows compatible ! *)
let relative_path =
  let split_path str = OcpString.split str '/' in
  let rec make_relative = function
    | (dir1::root, dir2::file) when dir1 = dir2 -> make_relative (root, file)
    | (root, file) ->
      List.fold_left (fun path _ -> Filename.parent_dir_name::path) file root
  in
  fun root file ->
    make_relative (split_path root, split_path file)
    |> String.concat Filename.dir_sep

(* [find_root root file] tries to find if [file] is present somewhere in the
   direct hierarchy of [root]. *)
let find_root root_dir basenames =
  let rec find dirname (basenames : string list) =
    let file = FileGen.add_basenames dirname basenames in
    if FileGen.exists file then dirname else
      let new_dirname = FileGen.dirname dirname in
      if new_dirname == dirname then raise Not_found;
      find new_dirname basenames
  in
  let root_dir = if FileGen.is_absolute root_dir then root_dir else
      FileGen.concat (FileGen.getcwd ()) root_dir
  in
  find root_dir basenames

(* Beware: not Windows compatible ! *)
let diff to_root path =
  let to_root = OcpString.split to_root '/' in
  let path = OcpString.split path '/' in
  let rec loop to_root path =
    match to_root, path with
    | dir1 :: tl1, dir2 :: tl2 when dir1 = dir2 -> loop tl1 tl2
    | _ -> String.concat "/" path
  in
  loop to_root path

let absolute_path file =
  let file_t = FileGen.of_string file in
  if FileGen.is_absolute file_t then (FileGen.to_string file_t)
  else
    let file = FileGen.concat (FileGen.getcwd ()) file_t in
    FileGen.to_string file

let absolute_path root path =
  let path_to_root = relative_path root (Sys.getcwd()) in
  let new_path = diff path_to_root path in
  absolute_path new_path

let normalize_path ~root ~file =
  relative_path root (absolute_path root file)

let is_in_path root file path =
  let path = normalize_path root path in
  let file = normalize_path root file in
  Str.string_match (Str.regexp path) file 0

let read_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s

let db_hash file =
  let file_content = read_file file in
  let string_to_hash = file ^ file_content in
  Digest.string string_to_hash

(* Code from Opam *)

let temp_basename prefix =
  Printf.sprintf "%s-%d-%06x" prefix (Unix.getpid ()) (Random.int 0xFFFFFF)

let rec mk_temp_dir prefix =
  let s =
    Filename.concat (Filename.get_temp_dir_name ()) (temp_basename prefix) in
  if Sys.file_exists s then
    mk_temp_dir prefix
  else
    s

let split_sources sources =
  let open Cmt_format in
  let cmts =
    List.filter (fun file -> Filename.check_suffix file ".cmt") sources in
  List.fold_left (fun acc cmt ->
      try
        let cmt_infos = read_cmt cmt in
        match cmt_infos.cmt_sourcefile, cmt_infos.cmt_source_digest with
        | Some src_file, Some src_digest -> (src_file, src_digest, cmt) :: acc
        | _ -> acc
      with exn -> acc)
    [] cmts,
  List.filter (fun file -> not (Filename.check_suffix file ".cmt")) sources

let find_cmt cmts_infos file =
  let hash = Digest.file file in
  let matching_file =
    List.filter (fun (sourcefile, digest, cmt) ->
        digest = hash &&
        (Filename.basename sourcefile) = (Filename.basename file))
      cmts_infos in
  match matching_file with
  | (_cmt_src_file, _cmt_digest, cmt):: tl -> Some cmt
  | [] -> None

let mk_file_struct root file ignored cmts_infos =
  let file_norm =
    if root <> "." then
      normalize_path root file
    else file in
  let hash = db_hash file in
  let cmt =
    if cmts_infos = [] then
      None
    else
      find_cmt cmts_infos file in
  { name = file; norm = file_norm; hash; ignored = []; cmt = cmt }

let save_file_struct dir fstruct =
  let file =  temp_basename "ocp-lint-file" in
  let path = Filename.concat dir file in
  let oc = open_out path in
  output_value oc fstruct;
  close_out oc;
  path

let read_file_struct file =
  let ic = open_in file in
  let fstruct = input_value ic in
  close_in ic;
  fstruct

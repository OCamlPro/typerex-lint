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


let subsitute str substs =
  let replace substs str = try List.assoc str substs with Not_found -> str in
  let buf = Buffer.create (String.length str) in
  Buffer.add_substitute buf (replace substs) str;
  Buffer.contents buf

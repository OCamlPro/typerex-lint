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
open Db_types

let print_warning ppf warning =
  if warning.loc <> Location.none then
    Format.fprintf ppf "%a" Location.print warning.loc;

  Format.fprintf ppf "  %s" warning.output;
  Format.fprintf ppf "@."

let print fmt db =
  Hashtbl.iter (fun file (hash, pres) ->
      StringMap.iter (fun pname lres ->
          StringMap.iter  (fun lname (_source, _opt, ws) ->
              let filters =
                Globals.Config.get_option_value [pname; lname; "warnings"] in
              let arr = Parse_args.parse_options filters in
              List.iter
                (fun warning ->
                   if arr.(warning.instance.id - 1) then
                     print_warning fmt warning)
                ws)
            lres)
        pres)
    db

let print_only_new fmt db =
  Hashtbl.iter (fun file (hash, pres) ->
      StringMap.iter (fun pname lres ->
          StringMap.iter  (fun lname (source, _opt, ws) ->
              if source = Analyse then List.iter (print_warning fmt) ws)
            lres)
        pres)
    db

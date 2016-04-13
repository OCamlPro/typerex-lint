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

open Reports

let read_json json_file =
  Yojson.Basic.from_file json_file

let json_of_pos pos =
  let open Lexing in
  `Assoc
    [ ("pos_fname", `String pos.pos_fname);
      ("pos_lnum", `Int pos.pos_lnum);
      ("pos_bol", `Int pos.pos_bol);
      ("pos_cnum", `Int pos.pos_cnum) ]

let json_of_loc loc =
  let open Location in
  `Assoc
    [ ("loc_start", (json_of_pos loc.loc_start));
      ("loc_end", (json_of_pos loc.loc_end));
      ("loc_ghost", `Bool loc.loc_ghost) ]

let json_of_info info =
  let open Info in
  `Assoc
    [ ("name", `String info.name);
      ("details", `String info.details);
      ("cat", `String (Info.string_of_cat info.cat)) ]

let json_of_report report =
  `Assoc [ ("kind", `String (Reports.string_of_kind report.kind));
           ("loc", json_of_loc report.loc);
           ("info", json_of_info report.info);
           ("msg", `String report.msg) ]

let json_of_reports reports =
  let list = List.map (json_of_report) (StringSet.elements reports) in
  let tm = Unix.localtime (Unix.time ()) in
  let time_str =
    Printf.sprintf "%i-%02i-%02i_%i-%i"
      (tm.Unix.tm_year + 1900)
      (tm.Unix.tm_mon + 1)
      tm.Unix.tm_mday
      tm.Unix.tm_hour
      tm.Unix.tm_min in
  `Assoc  [ ("date", `String time_str);
            ("list", `List list) ]
 
let write_json ppf reports =
  let json = json_of_reports !reports in
  let json_str = Yojson.Basic.pretty_to_string json in
  Format.fprintf ppf "%s" json_str

let json reports file =
  let oc = open_out file in
  let ppf = Format.formatter_of_out_channel oc in
  write_json ppf reports

let print reports =
  write_json Format.err_formatter reports

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

type action =
| ActionNone
| ActionList
| ActionLoad of string

let action = ref ActionNone

let filters = ref ""

let json_output_file = ref None

let txt_output_file = ref None

let set_action new_action =
   if !action <> ActionNone then
     raise @@ Arg.Bad
       "Options --project or --list-warnings cannot be used together";
   action := new_action

let usage_msg =
  let name = Filename.basename Sys.argv.(0) in
  String.concat "\n" [
    "Usage:";
    Printf.sprintf "   %s [OPTIONS] --project DIR" name;
    Printf.sprintf "   %s [OPTIONS] --project DIR --warnings -4,-5" name;
    "";
  ]

let arg_spec = Arg.align [
    "--project", Arg.String (fun dir ->
        set_action (ActionLoad dir)
      ), "DIR   Give a project dir path";
    "-p", Arg.String (fun dir ->
        set_action (ActionLoad dir)
      ), "DIR   Give a project dir path";
    "--warnings", Arg.Set_string filters
    , "  Give details about warnings";
    "--list-warnings", Arg.Unit (fun () ->
        set_action ActionList
      ), " List of warnings";
    "--json", Arg.String (fun file -> json_output_file := Some file)
    ,"file   Give file where to print warning in json format";
    "--txt", Arg.String (fun file -> txt_output_file := Some file)
    ,"file   Give file where to print warning in json format";
  ]

let arg_spec = Arg.align arg_spec

let main () =
  Arg.parse arg_spec
    (fun s ->
       Printf.printf "Error: don't know what to do with %s\n%!" s;
       exit 1)
    usage_msg;

  match !action with
  | ActionLoad dir ->
    Ocplint_actions.scan ~filters:!filters ~json_file:!json_output_file ~txt_file:!txt_output_file dir
  | ActionList ->
    Ocplint_actions.list_warnings ();
    exit 0
  | ActionNone ->
    Arg.usage arg_spec usage_msg;
    exit 0

let () =
  main ()

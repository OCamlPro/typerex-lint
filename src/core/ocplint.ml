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

type action =
| ActionNone
| ActionList
| ActionLoad of string

let action = ref ActionNone
let exit_status = ref 0
let patches = ref []

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
    Printf.sprintf "   %s [OPTIONS] --project DIR --patches foo.md,bar.md" name;
    "";
  ]

let specs : (Arg.key * Arg.spec * Arg.doc) list ref = ref []

let add_spec ((cmd, _, _) as spec) =
  if List.for_all (fun (key, _, _) -> cmd <> key) !specs then
    specs := spec :: !specs

let () =
  specs := Arg.align [
      "--project", Arg.String (fun dir -> set_action (ActionLoad dir)),
    "DIR   Give a project dir path";

    "--list-warnings", Arg.Unit (fun () -> set_action ActionList),
    " List of warnings";

    "--warn-error", Arg.Unit (fun () ->
        exit_status := 1),
    " Every warning returns an error status code.";

    "--patches", Arg.String (fun files ->
        patches := (Str.split (Str.regexp ",") files)),
    " List of user defined lint with the patch format.";

    "--load", Arg.String (fun files ->
        let l = (Str.split (Str.regexp ",") files) in
        List.iter Dynlink.loadfile l;
        List.iter add_spec (Globals.Config.simple_args ())),
    " Load dynamically plugins with their corresponding 'cmxs' files."
  ]

let main () =
  (* Getting all options declared in all registered plugins. *)
  List.iter add_spec (Globals.Config.simple_args ());
  Arg.parse_dynamic specs
      (fun cmd ->
       Printf.printf "Error: don't know what to do with %s\n%!" cmd;
       exit 1)
    usage_msg;

  match !action with
  | ActionLoad dir ->
    Ocplint_actions.scan ~filters:"" dir !patches;
    Plugin.iter_plugins (fun plugin checks ->
      let module P = (val plugin : Plugin_types.PLUGIN) in
      if Warning.length P.warnings > 0 then exit !exit_status);
    exit 0 (* No warning, we can exit successfully *)
  | ActionList ->
    exit 0
  | ActionNone ->
    Arg.usage !specs usage_msg;
    exit 0

let () = main ()

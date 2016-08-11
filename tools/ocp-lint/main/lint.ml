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

open StringCompat (* for StringMap *)

type action =
  | ActionNone
  | ActionList
  | ActionInit
  | ActionSave
  | ActionLoadDir of string
  | ActionLoadFile of string

let action = ref ActionNone
let exit_status = ref 0
let output_text = ref None
let default_dir = "."
let print_only_new = ref false
let no_db = ref false
let db_dir = ref None
let config_file = ref ""
let verbose = ref false
let severity_limit = ref 0
let good_plugins = ref []

module ArgAlign = struct
  open Arg

  let second_word s =
    let len = String.length s in
    let rec loop n =
      if n >= len then len
      else if s.[n] = ' ' then loop (n+1)
      else n
    in
    try loop (String.index s ' ')
    with Not_found -> len
  ;;

  let max_arg_len cur (kwd, spec, doc) =
    match spec with
    | Symbol _ -> max cur (String.length kwd)
    | _ -> max cur (String.length kwd + second_word doc)
  ;;

  let split offset s =
    let b = Buffer.create (String.length s) in
    let rec iter space pos list =
      match list with
      | [] -> ()
      | token :: tail ->
        let add = String.length token in
        if pos + add + space > 80 then begin
          Buffer.add_char b '\n';
          Buffer.add_string b (String.make offset ' ');
          iter 0 offset list
        end else begin
          if space = 1 then Buffer.add_char b ' ';
          Buffer.add_string b token;
          iter 1 (pos+space+add) tail
        end
    in
    let list = OcpString.split s ' ' in
    iter 0 offset list;
    Buffer.contents b

  let add_padding len (kwd,spec,msg) =
    let msg =
      if msg = "" then
        (* Do not pad undocumented options, so that they still don't
         * show up when run through [usage] or [parse]. *)
        ""
      else
        match spec with
        | Symbol (l, _) ->
          let cutcol = second_word msg in
          let spaces = String.make ((max 0 (len - cutcol)) + 3) ' ' in
          "\n" ^ spaces ^ msg
        | _ ->
          let cutcol = second_word msg in
          let kwd_len = String.length kwd in
          let diff = len - kwd_len - cutcol in
          if diff = 0 then
            msg
          else
          if diff < 0 then
            let prefix = String.sub msg 0 cutcol in
            let suffix = String.sub msg cutcol (String.length msg - cutcol) in
            let len = len+3 in
            let spaces = String.make len ' ' in
            prefix ^ "\n" ^ spaces ^ split len suffix
          else
            let spaces = String.make diff ' ' in
            let prefix = String.sub msg 0 cutcol in
            let suffix = String.sub msg cutcol (String.length msg - cutcol) in
            let len = len+3 in
            prefix ^ spaces ^ split len suffix
    in
    let kwd = OcpString.replace_chars kwd ['_', "-"] in
    (kwd, spec, msg)
  ;;

  let align ?(limit=max_int) speclist =
    let completed = (* add_help *) speclist in
    let len = List.fold_left max_arg_len 0 completed in
    let len = min len limit in
    let len = if len > 25 then 25 else len in
    List.map (add_padding len) completed

end

let set_action new_action =
  if !action <> ActionNone then
    raise @@ Arg.Bad
      "Options --path or --list-warnings cannot be used together";
  action := new_action

let usage_msg =
  let name = Filename.basename Sys.argv.(0) in
  String.concat "\n" [
    "Usage:";
    Printf.sprintf "   %s [OPTIONS] --path DIR" name;
    "";
  ]

let specs : (Arg.key * Arg.spec * Arg.doc) list ref = ref []
let static_args = ref []
let dynamic_args = ref []

let print_help () =
  Arg.usage !specs usage_msg;
  exit 0

let set_spec () =
  specs := ArgAlign.align !static_args @
           ArgAlign.align !dynamic_args @
           [ "", Arg.Unit (fun () -> ()), " \nGetting Help:\n\n";
             "-help", Arg.Unit print_help,
             "       Display this list of options";
             "--help", Arg.Unit print_help,
             "      Display this list of options";
           ]

let add_simple_args () =
  let args_map = ref StringMap.empty in
  List.iter (function (key,_,_) as arg ->
      args_map := StringMap.add key arg !args_map
    ) !dynamic_args;
  List.iter (function (key,_,_) as new_arg ->
      if not ( StringMap.mem key !args_map ) then
        args_map := StringMap.add key new_arg !args_map
    ) (Lint_globals.Config.simple_args ());
  dynamic_args := List.map snd (StringMap.to_list !args_map);
  set_spec ()

let () =
  static_args := [
    "", Arg.Unit (fun () -> ()),
    " \nKernel arguments:\n";

    "--init", Arg.Unit (fun dir -> set_action ActionInit),
    " Initialise a project";

    "--path", Arg.String (fun dir ->
        if !config_file <> ""
        then Lint_actions.init_config_file !config_file
        else Lint_actions.init_config dir;
        set_action (ActionLoadDir dir)),
    "DIR   Give a project dir path";

    "--file", Arg.String (fun file ->
        if !config_file <> ""
        then Lint_actions.init_config_file !config_file
        else Lint_actions.init_config (Filename.dirname file);
        set_action (ActionLoadFile file)),
    "FILE   Give a file to lint";

    "--db-dir", Arg.String (fun dir -> db_dir := Some dir),
    "DIR   Give database directory";

    "--output-txt", Arg.String (fun file -> output_text := Some file),
    "FILE   Output results in a text file.";

    "--list", Arg.Unit (fun () -> set_action ActionList),
    " List of every plugins and linters.";

    "--warn-error", Arg.Unit (fun () -> exit_status := 1),
    " Every warning returns an error status code.";

    "--load-plugins", Arg.String (fun files ->
        let l = (Str.split (Str.regexp ",") files) in
        good_plugins := Lint_actions.load_plugins l;
        add_simple_args ();
      ),
    "PLUGINS Load dynamically plugins with their corresponding 'cmxs' files.";

    "--save-config", Arg.Unit (fun () -> set_action ActionSave),
    " Save ocp-lint default config file.";

    "--load-config", Arg.String (fun file -> config_file := file),
    "FILE Specify which config file ocp-lint should use.";

    "--no-db-cache", Arg.Set no_db,
    " Ignore the database.";

    "--print-only-new", Arg.Unit (fun () -> print_only_new := true),
    " Print only new warnings.";

    "--severity-limit", Arg.Int (fun i -> severity_limit := i),
    " Only display warning above this severity";

    "--verbose", Arg.Set verbose,
    " Show extra informations.";

    "", Arg.Unit (fun () -> ()),
    " \n\nPlugins arguments:\n";
  ]

let start_lint_file file =
  Lint_actions.lint_file !verbose !no_db !db_dir !severity_limit file

let start_lint dir =
  let master_config = Filename.temp_file ".ocplint" "" in
  Lint_globals.Config.save_master master_config;
  Lint_actions.lint_sequential
    !no_db
    !db_dir
    !severity_limit
    !good_plugins
    master_config
    dir;
  if Lint_db.DefaultDB.has_warning () then exit !exit_status

let main () =
  (* Getting all options declared in all registered plugins. *)
  Lint_actions.init_config default_dir;
  add_simple_args ();
  Arg.parse_dynamic specs
    (fun cmd ->
       Printf.printf "Error: don't know what to do with %s\n%!" cmd;
       exit 1)
    usage_msg;

  match !action with
  | ActionLoadDir dir ->
    start_lint dir;
    exit 0 (* No warning, we can exit successfully *)
  | ActionLoadFile file ->
    start_lint_file file;
    exit 0 (* No warning, we can exit successfully *)
  | ActionList ->
    Lint_actions.list_plugins Format.std_formatter;
    exit 0
  | ActionInit ->
    Lint_globals.Config.save ();
    Lint_actions.init_olint_dir ()
  | ActionSave ->
    Lint_globals.Config.save ();
    exit 0
  | ActionNone ->
    start_lint default_dir;
    exit 0

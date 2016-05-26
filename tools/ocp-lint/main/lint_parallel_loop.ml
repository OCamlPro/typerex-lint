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

open SimpleConfig

let get_ignored_files pname cname =
  let opt =
    Lint_globals.Config.create_option [pname; cname; "ignored_files"]  "" "" 0
      (SimpleConfig.list_option SimpleConfig.string_option)  [] in
  !!opt

let is_in_ignored_files file files =
  List.exists (fun source -> source = file) files

let lint all mls mlis asts_ml asts_mli cmts plugins =
  let fmt = Format.err_formatter in

  Lint_db.DefaultDB.clean all;

  (* Itering on all files in your project *)
  Lint_plugin.iter_plugins (fun plugin checks ->
      Lint_map.iter (fun cname lint ->
          let module Plugin = (val plugin : Lint_plugin_types.PLUGIN) in
          let module Lint = (val lint : Lint_types.LINT) in
          List.iter (function
              | Lint_input.InAll main ->
                begin
                  try
                    main all
                  with Lint_plugin_error.Plugin_error err ->
                    Lint_plugin_error.print fmt err
                end
              | _ -> ()) Lint.inputs) checks) plugins;

  (* Itering on ml sources *)
  List.iter (fun input ->
      Lint_plugin.iter_plugins (fun plugin checks ->
          Lint_map.iter (fun cname lint ->
              let module Plugin = (val plugin : Lint_plugin_types.PLUGIN) in
              let module Lint = (val lint : Lint_types.LINT) in
              let ignored_files = get_ignored_files Plugin.short_name cname in
              if not (is_in_ignored_files input ignored_files) then
                List.iter (function
                    | Lint_input.InMl main ->
                      begin
                        if not
                            (Lint_db.DefaultDB.already_run input
                               Plugin.short_name cname) then
                          (Lint_db.DefaultDB.add_entry
                             input Plugin.short_name cname;
                           try
                             main input
                           with Lint_plugin_error.Plugin_error err ->
                             Lint_plugin_error.print fmt err)
                      end
                    | _ -> ()) Lint.inputs) checks)
        plugins)
    mls;

  (* Itering on mli sources *)
  List.iter (fun input ->
      Lint_plugin.iter_plugins (fun plugin checks ->
          Lint_map.iter (fun cname lint ->
              let module Plugin = (val plugin : Lint_plugin_types.PLUGIN) in
              let module Lint = (val lint : Lint_types.LINT) in
              let ignored_files = get_ignored_files Plugin.short_name cname in
              if not (is_in_ignored_files input ignored_files) then
                List.iter (function
                    | Lint_input.InMli main ->
                      begin
                        if not
                            (Lint_db.DefaultDB.already_run
                               input Plugin.short_name cname) then
                          (Lint_db.DefaultDB.add_entry
                             input Plugin.short_name cname;
                           try
                             main input
                           with Lint_plugin_error.Plugin_error err ->
                             Lint_plugin_error.print fmt err)
                      end
                    | _ -> ()) Lint.inputs) checks)
        plugins)
    mlis;

  (* Itering on Parsetree.structure *)
  List.iter (fun (file, input) ->
      Lint_plugin.iter_plugins (fun plugin checks ->
          Lint_map.iter (fun cname lint ->
              let module Plugin = (val plugin : Lint_plugin_types.PLUGIN) in
              let module Lint = (val lint : Lint_types.LINT) in
              let ignored_files = get_ignored_files Plugin.short_name cname in
              if not (is_in_ignored_files file ignored_files) then
                List.iter (function
                    | Lint_input.InStruct main ->
                      begin match Lazy.force input with
                        | None -> ()
                        | Some input ->
                          begin
                            if not
                                (Lint_db.DefaultDB.already_run file
                                   Plugin.short_name cname) then
                              (Lint_db.DefaultDB.add_entry
                                 file Plugin.short_name cname;
                               try
                                 main input
                               with
                               | Lint_db_error.Db_error err ->
                                 Lint_db_error.print fmt err
                               | Sempatch.Failure.SempatchException e ->
                                 Format.fprintf fmt "Sempatch error : %s\n"
                                   (Sempatch.Failure.to_string e)
                               | Lint_plugin_error.Plugin_error err ->
                                 Lint_plugin_error.print fmt err)
                          end
                      end
                    | _ -> ()) Lint.inputs) checks)
        plugins)
    asts_ml;

  (* Itering on Parsetree.signature *)
  List.iter (fun (file, input) ->
      Lint_plugin.iter_plugins (fun plugin checks ->
          Lint_map.iter (fun cname lint ->
              let module Plugin = (val plugin : Lint_plugin_types.PLUGIN) in
              let module Lint = (val lint : Lint_types.LINT) in
              let ignored_files = get_ignored_files Plugin.short_name cname in
              if not (is_in_ignored_files file ignored_files) then
                List.iter (function
                    | Lint_input.InInterf main ->
                      begin match Lazy.force input with
                        | None -> ()
                        | Some input ->
                          begin
                            if not
                                (Lint_db.DefaultDB.already_run
                                   file Plugin.short_name cname) then
                              (Lint_db.DefaultDB.add_entry file
                                 Plugin.short_name cname;
                               try
                                 main input
                               with Lint_plugin_error.Plugin_error err ->
                                 Lint_plugin_error.print fmt err)
                          end
                      end
                    | _ -> ()) Lint.inputs) checks)
        plugins)
    asts_mli;

  (* Itering on cmts *)
  List.iter (fun (file, input) ->
      Lint_plugin.iter_plugins (fun plugin checks ->
          Lint_map.iter (fun cname lint ->
              let module Plugin = (val plugin : Lint_plugin_types.PLUGIN) in
              let module Lint = (val lint : Lint_types.LINT) in
              let ignored_files = get_ignored_files Plugin.short_name cname in
              if not (is_in_ignored_files file ignored_files) then
                List.iter (function
                    | Lint_input.InCmt main ->
                      begin
                        if not
                            (Lint_db.DefaultDB.already_run
                               file Plugin.short_name cname) then
                          (Lint_db.DefaultDB.add_entry
                             file Plugin.short_name cname;
                           try
                             main (Lazy.force input)
                           with Lint_plugin_error.Plugin_error err ->
                             Lint_plugin_error.print fmt err)
                      end
                    | _ -> ()) Lint.inputs) checks)
        plugins)
    cmts

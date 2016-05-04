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

let lint all mls mlis asts_ml asts_mli cmts plugins =
  let fmt = Format.err_formatter in
  (* Itering on all files in your project *)
  Plugin.iter_plugins (fun plugin checks ->
      Globals.LintMap.iter (fun cname runs ->
          List.iter (function
              | Input.InAll main ->
                begin
                  try
                    main all
                  with Plugin_error.Plugin_error err ->
                    Plugin_error.print fmt err
                end
              | _ -> ()) runs) checks) plugins;

  (* Itering on ml sources *)
  List.iter (fun input ->
      Plugin.iter_plugins (fun plugin checks ->
          Globals.LintMap.iter (fun cname runs ->
              List.iter (function
                  | Input.InMl main ->
                    begin
                      try
                        main input
                      with Plugin_error.Plugin_error err ->
                        Plugin_error.print fmt err
                    end
                  | _ -> ()) runs) checks)
        plugins)
    mls;

  (* Itering on mli sources *)
  List.iter (fun input ->
      Plugin.iter_plugins (fun plugin checks ->
          Globals.LintMap.iter (fun cname runs ->
              List.iter (function
                  | Input.InMli main ->
                    begin
                      try
                        main input
                      with Plugin_error.Plugin_error err ->
                        Plugin_error.print fmt err
                    end
                  | _ -> ()) runs) checks)
        plugins)
    mlis;

  (* Itering on Parsetree.structure *)
  List.iter (function input ->
      Plugin.iter_plugins (fun plugin checks ->
          Globals.LintMap.iter (fun cname runs ->
              List.iter (function
                  | Input.InStruct main ->
                    begin match Lazy.force input with
                      | None -> ()
                      | Some input ->
                        begin
                          try
                            main input
                          with
                          | Sempatch.Failure.SempatchException e ->
                            Format.fprintf fmt "Sempatch error : %s\n"
                              (Sempatch.Failure.to_string e)
                          | Plugin_error.Plugin_error err ->
                            Plugin_error.print fmt err
                        end
                    end
                  | _ -> ()) runs) checks)
        plugins)
    asts_ml;

  (* Itering on Parsetree.signature *)
  List.iter (function input ->
      Plugin.iter_plugins (fun plugin checks ->
          Globals.LintMap.iter (fun cname runs ->
              List.iter (function
                  | Input.InInterf main ->
                    begin match Lazy.force input with
                      | None -> ()
                      | Some input ->
                        begin
                          try
                            main input
                          with Plugin_error.Plugin_error err ->
                            Plugin_error.print fmt err
                        end
                    end
                  | _ -> ()) runs) checks)
        plugins)
    asts_mli;

  (* Itering on cmts *)
  List.iter (fun input ->
      Plugin.iter_plugins (fun plugin checks ->
          Globals.LintMap.iter (fun cname runs ->
              List.iter (function
                  | Input.InCmt main ->
                    begin
                      try
                        main (Lazy.force input)
                      with Plugin_error.Plugin_error err ->
                        Plugin_error.print fmt err
                    end
                  | _ -> ()) runs) checks)
        plugins)
    cmts

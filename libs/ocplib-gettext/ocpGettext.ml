(**************************************************************************)
(*                                                                        *)
(*                              OCamlPro TypeRex                          *)
(*                                                                        *)
(*   Copyright OCamlPro 2011-2016. All rights reserved.                   *)
(*   This file is distributed under the terms of the LGPL v2.1 with       *)
(*   the special exception on linking described in the file LICENSE.      *)
(*      (GNU Lesser General Public Licence version 2.1)                   *)
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

open StringCompat
open SimpleConfig

let libraries = ref []
let program = ref
    (let basename = Filename.basename Sys.executable_name in
     let basename, _ = OcpString.cut_at basename '.' in
     String.lowercase basename
    )

let initialized = ref false

let exe_dirname = Filename.dirname Sys.executable_name
let exe_dirname =
  if Filename.is_implicit exe_dirname || Filename.is_relative exe_dirname then
    Filename.concat (Sys.getcwd ()) exe_dirname
  else exe_dirname
let prefix = Filename.dirname exe_dirname
let default_translations_path =
  if Filename.basename prefix = "_obuild" then
    [Filename.concat (Filename.dirname prefix) "i18n"]
  else
    [ Filename.concat prefix "lib/ocaml";
      Filename.concat prefix "lib" ]

let default_translation_path modname =
  let modname = Filename.concat modname "i18n" in
  List.map (fun prefix ->
      Filename.concat prefix modname)
    default_translations_path

let translation_path = ref default_translation_path

let translation_var = ref None
let translation_langs = ref (
    try
      let lang = Sys.getenv "LANG" in
      let lang,_ = OcpString.cut_at lang '.' in
      let lang, dialect = OcpString.cut_at lang '_' in
      let lang = String.lowercase lang in
      if dialect = "" then
        [ lang ]
      else
        [ Printf.sprintf "%s_%s" lang (String.uppercase dialect);
          lang ]
    with Not_found -> [ "en_US"; "en" ]
  )

let map = ref StringMap.empty

let update_translations_file = ref None

let translation_config = SimpleConfig.create_config_file (FileGen.of_string ".")

let translations = SimpleConfig.create_option translation_config
    [ "translations" ]
    ~short_help:""
    [ "Translations" ]
    (SimpleConfig.list_option (SimpleConfig.tuple2_option
                                 (SimpleConfig.string_option,
                                  SimpleConfig.string_option)))
    []

let missing_translations = SimpleConfig.create_option translation_config
    [ "missing_translations" ]
    ~short_help:""
    [ "Strings missing a translation in this file" ]
    (SimpleConfig.list_option SimpleConfig.string_option)
    []

module Op = struct
let _s s =
  if not !initialized then begin
    Printf.eprintf
      "Warning: translation for %S requested before full initialization\n%!" s;
    s
  end else
    try StringMap.find s !map
    with
    | Not_found ->
      begin match !update_translations_file with
          None -> ()
        | Some _ ->
          map := StringMap.add s s !map;
          missing_translations =:= s :: !!missing_translations;
      end;
      s
end

let init ?(verbose=false) ?(path= !translation_path) ?var ?langs program_name =
  initialized := true;
  program := program_name;
  translation_path := path;
  begin match langs with
    | None -> ()
    | Some langs -> translation_langs := langs
  end;
  begin
    match var with
    | None -> ()
    | Some var -> begin
        try
          let filename = Sys.getenv var in
          update_translations_file := Some filename
        with _ -> ()
      end
  end;
  List.iter (fun modname ->
      try
        let path = !translation_path modname in
        let rec iter = function
          | [] ->
            Printf.eprintf "Warning: no translation file for %S\n%!" modname
          | dir :: tail ->
            let filename = Filename.concat dir modname in
            iter_lang tail filename !translation_langs

        and iter_lang tail prefix = function
          | [] -> iter tail
          | lang :: langs ->
            let filename = prefix ^ "." ^ lang in
            if Sys.file_exists filename then begin
              translations =:= [];
              SimpleConfig.set_config_file translation_config
                (FileGen.of_string filename);
              SimpleConfig.load translation_config;
              List.iter (fun (s1, s2) ->
                  map := StringMap.add s1 s2 !map
                ) !!translations
            end else
              iter_lang tail prefix langs
        in
        iter path
      with exn ->
        if verbose then begin
          Printf.eprintf "Warning: could not load translations for %S\n%!"
            modname;
          Printf.eprintf "   Exception: %s\n%!" (Printexc.to_string exn)
        end
    ) (List.rev (!program :: !libraries));
  translations =:= StringMap.to_list !map;
  missing_translations =:= []

let library name = libraries := name :: !libraries

let () =
  at_exit (fun () ->
      match !update_translations_file with
      | None -> ()
      | Some config_file ->
        begin
          try
            SimpleConfig.set_config_file translation_config
              (FileGen.of_string config_file);
            SimpleConfig.save_with_help translation_config
          with exn ->
            Printf.eprintf "Warning: could not save translation file %S\n%!"
              config_file
        end
    )

let set_translations trs =
  List.iter (fun (s1,s2) ->
      map := StringMap.add s1 s2 !map
    ) trs

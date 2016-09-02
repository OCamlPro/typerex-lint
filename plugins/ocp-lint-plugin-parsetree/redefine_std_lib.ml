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

let details =
  "Checks that we do not redefine a stdlib module."

module RedefineStdLib = Plugin_parsetree.Plugin.MakeLint(struct
    let name = "Refedine Stdlib Module"
    let version = 1
    let short_name = "code_redefine_stdlib_module"
    let details = details
    let enable = true
  end)

type warning =
  | RedefinedStdLibs of string
  | RedefinedCplLibs of string

let w_redefined_std = RedefineStdLib.new_warning
    ~id:1
    ~short_name:"redfine_stdlib_module"
    ~msg:"$mod is a stdlib module and should not be masked."
    ~severity:7

let w_redefined_cpl = RedefineStdLib.new_warning
    ~id:2
    ~short_name:"redfine_compilerlib_module"
    ~msg:"$mod is a compilerlib module and should not be masked."
    ~severity:7

module Warnings = RedefineStdLib.MakeWarnings(struct
    type t = warning

    let to_warning = function
      | RedefinedStdLibs modname -> w_redefined_std, [("mod", modname)]
      | RedefinedCplLibs modname -> w_redefined_cpl, [("mod", modname)]

  end)

let is_stdlib_filename name =
  List.exists (fun mname ->
      (String.uppercase mname) = (String.uppercase name))
    Std_lib.std_lib_modules

let is_cpllib_filename name =
  List.exists (fun mname ->
      (String.uppercase mname) = (String.uppercase name))
    Std_lib.compiler_lib_modules

let is_stdlib_module mbind =
  let open Parsetree in
  let open Asttypes in
  let name = mbind.pmb_name.txt in
  List.exists (fun mname ->
      (String.uppercase mname) = (String.uppercase name))
    Std_lib.std_lib_modules

let is_cpllib_module mbind =
  let open Parsetree in
  let open Asttypes in
  let name = mbind.pmb_name.txt in
  List.exists (fun mname ->
      (String.uppercase mname) = (String.uppercase name))
    Std_lib.compiler_lib_modules

let iter =
  let module IterArg = struct
    include Parsetree_iter.DefaultIteratorArgument

    let enter_structure_item str =
      let open Parsetree in
      let open Asttypes in
      match str.pstr_desc with
      | Pstr_module mbind ->
        if is_stdlib_module mbind then
          Warnings.report
            mbind.pmb_name.loc (RedefinedStdLibs mbind.pmb_name.txt);
        if is_cpllib_module mbind then
          Warnings.report
            mbind.pmb_name.loc (RedefinedCplLibs mbind.pmb_name.txt)
      | Pstr_recmodule l ->
        List.iter (fun mbind ->
            if is_stdlib_module mbind then
              Warnings.report
                mbind.pmb_name.loc (RedefinedStdLibs mbind.pmb_name.txt);
            if is_cpllib_module mbind then
              Warnings.report
                mbind.pmb_name.loc (RedefinedCplLibs mbind.pmb_name.txt))
          l
      | _ -> ()

  end in
  (module IterArg : Parsetree_iter.IteratorArgument)

(* Registering a main entry to the linter *)

module MainStruct = RedefineStdLib.MakeInputStructure(struct
    let main ast = Parsetree_iter.iter_structure iter ast
  end)

module MainMl = RedefineStdLib.MakeInputML(struct
    let main filename =
      let modname = Filename.chop_extension (Filename.basename filename) in
      if is_stdlib_filename modname then
        Warnings.report_file
          filename (RedefinedStdLibs (String.capitalize modname))
      else
      if is_cpllib_filename modname then
        Warnings.report_file
          filename (RedefinedCplLibs (String.capitalize modname))
  end)

module MainMli = RedefineStdLib.MakeInputMLI(struct
    let main filename =
      let modname = Filename.chop_extension (Filename.basename filename) in
      if is_stdlib_filename modname then
        Warnings.report_file
          filename (RedefinedStdLibs (String.capitalize modname))
      else
      if is_cpllib_filename modname then
        Warnings.report_file
          filename (RedefinedCplLibs (String.capitalize modname))
  end)

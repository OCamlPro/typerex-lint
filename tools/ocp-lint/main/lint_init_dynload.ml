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


let init () =
  (* Core modules *)
  Findlib.record_package Findlib.Record_core "findlib";
  Findlib.record_package Findlib.Record_core "dynlink";
  Findlib.record_package Findlib.Record_core "findlib.dynload";
  Findlib.record_package Findlib.Record_core "compiler-libs.common";
  Findlib.record_package Findlib.Record_core "unix";
  Findlib.record_package Findlib.Record_core "str";
  Findlib.record_package Findlib.Record_core "ocplib-unix";
  Findlib.record_package Findlib.Record_core "ocp-lint-output";
  Findlib.record_package Findlib.Record_core "ocp-lint-config";
  Findlib.record_package Findlib.Record_core "ocp-lint-db";
  Findlib.record_package Findlib.Record_core "ocp-lint-init";
  Findlib.record_package Findlib.Record_core "ocp-lint-utils";

#if OCAML_VERSION >= "4.04.0"
  (match Sys.backend_type with
   | Sys.Native ->
     Findlib.record_package_predicates
       ["pkg_findlib";"pkg_dynlink";"pkg_findlib.dynload";"autolink";"native"]
   | Sys.Bytecode ->
     Dynlink.allow_unsafe_modules true;
     Findlib.record_package_predicates
       ["pkg_findlib";"pkg_dynlink";"pkg_findlib.dynload";"autolink";"byte"]
   | Sys.Other str -> ())
#else
  Findlib.record_package_predicates
    ["pkg_findlib";"pkg_dynlink";"pkg_findlib.dynload";"autolink";"native"]
#endif

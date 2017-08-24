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

(**
  The name of the JSON analysis result in the index
  **)
val analysis_info_var:
  string

(**
  The name of the JSON warnings list in the files contents
  **)
val warnings_info_var:
  string

(**
  The id of the code viewer element in the files contents
  **)
val web_code_viewer_id:
  string

(**
  The id of the animation element in the files contents
  **)
val web_code_loading_animation_id:
  string

(**
  Get the name of the generated static file page
  **)
val file_info_page :
  Lint_web_analysis_info.file_info ->
  string

(**
  Generate the web output
  **)
val generate_web_files :
  Format.formatter ->
  string ->
  (Lint_utils.file_struct * ((string list) * string)) list ->
  string ->
  Lint_db_types.t ->
  Lint_db_types.errors ->
  unit

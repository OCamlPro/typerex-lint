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

let default_patches =
  (* To add a static file, edit plugins/ocp-lint-patch/build.ocp *)
  List.map (fun (file, content) ->
      let tmp = Filename.get_temp_dir_name () in
      let file = Filename.basename file in
      let destfile = Filename.concat tmp file in
      File.Dir.make_all (File.of_string @@ Filename.dirname destfile);
      File.file_of_string destfile content;
      destfile)
    Global_static_files.files

module Default = Plugin_patch.PluginPatch.MakeLintPatch(struct
    let name = "Lint from semantic patches (default)"
    let version = 1
    let short_name = "sempatch_lint_default"
    let details = "Lint from semantic patches (default)."
    let patches = default_patches
    let enable = false
  end)

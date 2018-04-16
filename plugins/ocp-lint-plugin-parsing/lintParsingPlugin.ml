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

let plugin_name = "Parsing Linter"

let details = "Analyses requiring to re-parse the file"

module Plugin = Lint_plugin_api.MakePlugin (struct
    let name = plugin_name
    let short_name = "plugin_parsing"
    let details = details
    let enabled = true
  end)

let wrap_syntax_error main s =
  try
    main s
  with
  | LintParsing_Syntaxerr.Error error ->
     let s = LintParsing_Syntaxerr.prepare_error error in
     ignore (Format.flush_str_formatter ());
     LintParsing_Location.report_error Format.str_formatter s;
     let s = Format.flush_str_formatter () in
     raise (Lint_plugin_error.Plugin_error
              (Lint_plugin_error.Plugin_error s))

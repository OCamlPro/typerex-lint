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

module Op : sig

  (* [_s string] returns the translation of [string]. This call will never
     fail, returning its argument if no translation was found. *)
  val _s : string -> string

end

(* [init ?verbose ?path ?var ?langs modname] will load the translation
   files. You should not execute [Op._s] before using this function,
   as translations will not be available. [modname] will be used as
   the main basename for the file containing the translations for the
   program.

For example:
```
   init "my-program"
```
will lookup translation files, with LANG=en_US, such as
"my-program.en_US" or "my-program.en" in the translation path.

[verbose] can be used to tell [init] whether warnings should be
printed if translation files could not be loaded.

[path] is a function, returning, for each modname, a list of directories
where to search for translation files. By default, for a modname "foo" and
an executable in "${prefix}/bin", it will be
[ "${prefix}/lib/ocaml/foo/i18n"; "${prefix}/lib/foo/i18n" ] and
[ "${prefix}/i18n/foo" ] if the executable is in
"${prefix}/_obuild/${dstdir}/".

[translation_var]: if provided, the name of an environment variable that
can be set to save a translation file with missing translation, that
can be used to add more translations. Note that the file will contain
all translations and missing translations, including for libraries.

[langs]: a list of languages to use for the current
   execution. If None, the value is inferred from the LANG environment
   variable. If LANG is not set, it is  ["en_US"; "en" ].
*)

val init :
  ?verbose: bool ->
  ?path:(string -> string list) ->
  ?var:string ->
  ?langs:string list ->
  string ->
  unit


(* [library modname] This function should be used by a library to
   declare a possible basename [modname] for the files containing its
   translations. SimpleLang will look for such files in complement to
   the main program translations, so that it is easy for a library to
   provide its own translations. *)
val library : string -> unit

(* [set_translations translations] define a set of translations to be
   used during the current execution. Such translations can be user
   contributed translations from a user configuration file. *)
val set_translations : (string * string) list -> unit

(* This function is used internally to return, for a modname,
   the path where to search for translations. If a developer wants
   to provide his own [~path] to [init], he might want to use this
   function to create the end of the path. *)
val default_translation_path: string -> string list

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

let warning_max = 512
let active = Array.make warning_max true

let letter = function
  | 'a' ->
    let rec loop i = if i = 0 then [] else i :: loop (i - 1) in
    loop warning_max
  | 'c' | 't' | 'i'  | 'd' | 'e' | 'f' | 'b' | 'g' | 'h' | 'j' | 'k' | 'l' | 'm'
  | 'n' | 'o' | 'p' | 'q' | 'r' | 's' | 'u' | 'v' | 'w' | 'x' | 'y' | 'z' -> []
  | _ -> assert false

let parse_opt active flags s =
  let set i = flags.(i - 1) <- true in
  let clear i = flags.(i - 1) <- false in
  let set_all i = active.(i - 1) <- true in
  let error () = raise (Arg.Bad "Ill-formed list of warnings") in
  let rec get_num n i =
    if i >= String.length s then i, n
    else match s.[i] with
      | '0'..'9' -> get_num (10 * n + Char.code s.[i] - Char.code '0') (i + 1)
      | _ -> i, n
  in
  let get_range i =
    let i, n1 = get_num 0 i in
    if i + 2 < String.length s && s.[i] = '.' && s.[i + 1] = '.' then
      let i, n2 = get_num 0 (i + 2) in
      if n2 < n1 then error ();
      i, n1, n2
    else
      i, n1, n1
  in
  let rec loop i =
    if i < String.length s then
      match s.[i] with
      | 'A' .. 'Z' ->
        List.iter set (letter (Char.lowercase s.[i]));
        loop (i+1)
      | 'a' .. 'z' ->
        List.iter clear (letter s.[i]);
        loop (i+1)
      | '+' -> loop_letter_num set (i+1)
      | '-' -> loop_letter_num clear (i+1)
      | '@' -> loop_letter_num set_all (i+1)
      | c -> error ()
  and loop_letter_num myset i =
    if i >= String.length s then error () else
      match s.[i] with
      | '0' .. '9' ->
        let i, n1, n2 = get_range i in
        for n = n1 to min n2 warning_max do myset n done;
        loop i
      | 'A' .. 'Z' ->
        List.iter myset (letter (Char.lowercase s.[i]));
        loop (i+1)
      | 'a' .. 'z' ->
        List.iter myset (letter s.[i]);
        loop (i+1)
      | _ -> error ()
  in
  loop 0

let parse_options s =
  Array.iteri (fun i _ -> active.(i) <- true) active;
  parse_opt active active s;
  active

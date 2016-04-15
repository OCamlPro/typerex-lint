open Check_types
open Info

let checks : Check_types.check list = [

  (* Code *)
  1, GCheck Code_length.check;
  2, SCheck Code_identifier_length.check;
  3, SCheck Code_identity_let.check;

  (* Typo *)
  4, GCheck Interface_missing.check;

  (* Interface *)
  5, CCheck Test_cmt.check;

  (* Doc *)
]

let global_checks checks =
  List.fold_left (fun checks (_, check) ->
      match check with
      | GCheck chk -> chk :: checks
      | _ -> checks)
    [] checks

let source_checks checks =
  List.fold_left (fun checks (_, check) ->
      match check with
      | SCheck chk -> chk :: checks
      | _ -> checks)
    [] checks

let cmt_checks checks =
  List.fold_left (fun checks (_, check) ->
      match check with
      | CCheck chk -> chk :: checks
      | _ -> checks)
    [] checks

let filter cat =
  let rec loop cat acc = function
    | [] -> acc
    | (_, check) as c  :: checks ->
      match check with
    | GCheck chk ->
      let acc = if chk.global_info.cat = cat then c :: acc else acc in
      loop cat acc checks
    | SCheck chk ->
      let acc = if chk.source_info.cat = cat then c :: acc else acc in
      loop cat acc checks
    | CCheck chk ->
      let acc = if chk.cmt_info.cat = cat then c :: acc else acc in
      loop cat acc checks in
  loop cat [] checks

(* Code taken from OCaml/utils/warnings.ml *)
let last_warning_number = List.length checks

let letter = function
  | 'a' ->
    let rec loop i = if i = 0 then [] else i :: loop (i - 1) in
    loop last_warning_number
  | 'c' -> fst @@ List.split @@ filter Info.Code
  | 't' -> fst @@ List.split @@ filter Info.Typo
  | 'i' -> fst @@ List.split @@ filter Info.Interface
  | 'd' | 'e' | 'f' | 'b' | 'g' | 'h' | 'j' | 'k' | 'l' | 'm' | 'n'
  | 'o' | 'p' | 'q' | 'r' | 's' | 'u' | 'v' | 'w' | 'x' | 'y' | 'z' -> []
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
    if i >= String.length s then () else
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
        for n = n1 to min n2 last_warning_number do myset n done;
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

let parse_options errflag s =
  let active = Array.make (last_warning_number) true in
  parse_opt active active s;
  active

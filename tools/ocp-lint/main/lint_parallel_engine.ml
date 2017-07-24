type proc_status = Waiting | Running of int | Done

let jobs_pool = Hashtbl.create 100

let remaining_files = ref 0

let find_next_waiting () =
  let elt =
    Hashtbl.fold (fun k v file ->
        if file <> None then
          file
        else
          match v with
          | Waiting -> Some k
          | _ -> file
      ) jobs_pool None in
  match elt with
  | None -> raise Not_found
  | Some file -> file

let waiting_file () = !remaining_files > 0

let done_files total_files =
  total_files - !remaining_files

let mark_waiting sources =
  List.iter (fun file -> Hashtbl.add jobs_pool file Waiting) sources;
  let len =  List.length sources in
  remaining_files := len

let mark_running file pid =
  Hashtbl.replace jobs_pool file (Running pid)

let mark_done pid =
  let elt = Hashtbl.fold (fun k v file ->
      if file <> None then
        file
      else
        match v with
        | Running i when i = pid -> Some k
        | _ -> file
    ) jobs_pool None in
  match elt with
  | None -> ()
  | Some file ->
    Hashtbl.replace jobs_pool file Done;
    decr remaining_files

let get_start_list jobs =
  let rec loop cpt acc = function
    | [] -> acc
    | hd :: tl ->
      if cpt = jobs then
        acc
      else loop (cpt + 1) (hd :: acc) tl
  in loop 0 []

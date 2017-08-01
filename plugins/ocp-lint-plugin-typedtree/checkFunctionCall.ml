let unsafe_functions = [
  Path.Pdot (
      Path.Pdot (
          Path.Pident (
              Ident.create "Bigarray"
            ), "Array1", 1
        ), "unsafe_get", 2
    );
  Path.Pdot (
      Path.Pdot (
          Path.Pident (
              Ident.create "Bigarray"
            ), "Array1", 1
        ), "unsafe_set", 2
    );
  Path.Pdot (
      Path.Pdot (
          Path.Pident (
              Ident.create "Bigarray"
            ), "Array2", 1
        ), "unsafe_get", 2
    );
  Path.Pdot (
      Path.Pdot (
          Path.Pident (
              Ident.create "Bigarray"
            ), "Array2", 1
        ), "unsafe_set", 2
    );
  Path.Pdot (
      Path.Pdot (
          Path.Pident (
              Ident.create "Bigarray"
            ), "Array3", 1
        ), "unsafe_get", 2
    );
  Path.Pdot (
      Path.Pdot (
          Path.Pident (
              Ident.create "Bigarray"
            ), "Array3", 1
        ), "unsafe_set", 2
    );
  Path.Pdot (
      Path.Pident (
          Ident.create "Bytes"
        ), "unsafe_to_string", 1
    );
  Path.Pdot (
      Path.Pdot (
          Path.Pident (
              Ident.create "Misc"
            ), "LongString", 1
        ), "unsafe_blit_to_byte", 2
    );
]

let dynamic_linking_functions = [
  Path.Pdot (
      Path.Pident (
          Ident.create "Dynlink"
        ), "reset", 1
    );
  Path.Pdot (
      Path.Pident (
          Ident.create "Dynlink"
        ), "add_interfaces", 1
    );
  Path.Pdot (
      Path.Pident (
          Ident.create "Dynlink"
        ), "add_variables_units", 1
    );
  Path.Pdot (
      Path.Pident (
          Ident.create "Dynlink"
        ), "loadfile", 1
    );
  Path.Pdot (
      Path.Pident (
          Ident.create "Dynlink"
        ), "loadfile_private", 1
    );
]

let deprecated_functions = [ (* (deprecated * actual) list *)
  (Path.Pdot (
        Path.Pident (
            Ident.create "String"
          ), "create", 1
      )
  ,Path.Pdot (
        Path.Pident (
            Ident.create "String"
          ), "make", 1
      )
  );
]

let rec same_path p1 p2 =
  match (p1, p2) with
  | Path.Pident id1, Path.Pident id2 ->
     Ident.equal id1 id2
  | Path.Pdot(p1, s1, _pos1), Path.Pdot(p2, s2, _pos2) ->
     s1 = s2 && same_path p1 p2
  | Path.Papply(fun1, arg1), Path.Papply(fun2, arg2) ->
     same_path fun1 fun2 && same_path arg1 arg2
  | (_, _) -> false

module Linter = Plugin_typedtree.Plugin.MakeLint(struct
    let name = "Function Call"
    let version = "1"
    let short_name = "function_call"
    let details = "Checks some properties of function calls"
    let enable = true
  end)

type warning =
  | UseUnsafeFunction of Path.t
  | UseDynamicLinkingFunction of Path.t
  | UseDeprecatedFunction of Path.t * Path.t

let w_unsafe_fun = Linter.new_warning
    ~id:1
    ~short_name:"unsafe_function"
    ~msg:"$fun is an unsafe function."
    ~severity:1

let w_dynamic_linking_fun = Linter.new_warning
    ~id:2
    ~short_name:"dynamic_linking_function"
    ~msg:"$fun is a dynamic linking function."
    ~severity:1

let w_deprecated_fun = Linter.new_warning
    ~id:3
    ~short_name:"deprecated_function"
    ~msg:"$old is a deprecated function, use $new."
    ~severity:1

module Warnings = Linter.MakeWarnings(struct
    type t = warning

    let to_warning = function
      | UseUnsafeFunction p ->
	 w_unsafe_fun, [("fun",Path.name p)]
      | UseDynamicLinkingFunction p ->
	 w_dynamic_linking_fun, [("fun",Path.name p)]
      | UseDeprecatedFunction (p,p') ->
	 w_deprecated_fun, [("old",Path.name p);("new", Path.name p')]
end)

let iter =
  let module IterArg = struct
    include Typedtree_iter.DefaultIteratorArgument

    let is_unsafe fpath =
      List.exists (same_path fpath) unsafe_functions

    let is_dynamic_linking_call fpath =
      List.exists (same_path fpath) dynamic_linking_functions

    let actual_of_deprecated fpath =
      let rec aux = function
        | [] -> None
        | (df,nf) :: _ when same_path fpath df -> Some nf
        | _ :: tl -> aux tl
      in aux deprecated_functions

    let process_function_call fpath loc =
      if is_unsafe fpath then begin
         Warnings.report loc (UseUnsafeFunction fpath)
      end;
      if is_dynamic_linking_call fpath then begin
         Warnings.report loc (UseDynamicLinkingFunction fpath)
      end;
      match actual_of_deprecated fpath with
      | None ->
         ()
      | Some actual ->
         Warnings.report loc (UseDeprecatedFunction (fpath, actual))

    let enter_expression expr =
      let open Typedtree in
      let open Asttypes in
      begin match expr.exp_desc with
      | Texp_apply (f, _) ->
	 begin match f.exp_desc with
         | Texp_ident (path, _, _) -> process_function_call path f.exp_loc
         | _ -> ()
	 end
      | _ -> ()
      end

  end in
  (module IterArg : Typedtree_iter.IteratorArgument)

module MainML = Linter.MakeInputCMT(struct
    let main cmt = Typedtree_iter.iter_structure iter cmt
  end)

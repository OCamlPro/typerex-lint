module Plugin = LintParsingPlugin.Plugin

type match_type =
  | SimpleMatch
  | MatchException
		  
module Linter = Plugin.MakeLint(struct
    let name = "Get Features"
    let version = "1"
    let short_name = "get_features"
    let details = "Extract the features of the language"
    let enable = true
  end)

type warning =
  | UseClass
  | UseTryWith
  | UseMatch
  | UseExceptionPat
  | UseWhile
  | UseFor
  | UseIfThenElse
  | UseIfThen
  | UseMethod
  | UseLocalOpen
  | UseLocalException
  | UseUnpack
    
let w_use_class = Linter.new_warning
    ~id:1
    ~short_name:"use_class"
    ~msg:"Use class"
    ~severity:1

let w_use_try_with = Linter.new_warning
    ~id:2
    ~short_name:"use_try_with"
    ~msg:"Use try ... with ..."
    ~severity:1

let w_use_match = Linter.new_warning
    ~id:3
    ~short_name:"use_match"
    ~msg:"Use match ... with ..."
    ~severity:1

let w_use_match_exception = Linter.new_warning
    ~id:4
    ~short_name:"use_match_exception"
    ~msg:"Use exception pattern"
    ~severity:1

let w_use_while = Linter.new_warning
    ~id:5
    ~short_name:"use_while"
    ~msg:"Use while ... do ... done"
    ~severity:1

let w_use_for = Linter.new_warning
    ~id:6
    ~short_name:"use_for"
    ~msg:"Use for ... do ... done"
    ~severity:1

let w_use_if_then_else = Linter.new_warning
    ~id:7
    ~short_name:"use_if_then_else"
    ~msg:"Use if ... then ... else ..."
    ~severity:1

let w_use_if_then = Linter.new_warning
    ~id:8
    ~short_name:"use_if_then"
    ~msg:"Use if ... then ..."
    ~severity:1

let w_use_method = Linter.new_warning
    ~id:9
    ~short_name:"use_method"
    ~msg:"Use method ...#..."
    ~severity:1
    
let w_use_local_open = Linter.new_warning
    ~id:10
    ~short_name:"use_local_open"
    ~msg:"Use M.(...)"
    ~severity:1

let w_use_local_exception = Linter.new_warning
    ~id:11
    ~short_name:"use_local_exception"
    ~msg:"Use let exception ... in ..."
    ~severity:1

let w_use_unpack = Linter.new_warning
    ~id:12
    ~short_name:"use_unpack"
    ~msg:"Use first-class module unpacking"
    ~severity:1
    
module Warnings = Linter.MakeWarnings(struct
    type t = warning

    let to_warning = function
      | UseClass -> w_use_class, []
      | UseTryWith -> w_use_try_with, []
      | UseMatch -> w_use_match, []
      | UseExceptionPat -> w_use_match_exception, []
      | UseWhile -> w_use_while, []
      | UseFor -> w_use_for, []
      | UseIfThenElse -> w_use_if_then_else, []
      | UseIfThen -> w_use_if_then, []
      | UseMethod -> w_use_method, []
      | UseLocalOpen -> w_use_local_open, []
      | UseLocalException -> w_use_local_exception, []
      | UseUnpack -> w_use_unpack, []
  end)

module Asttypes = LintParsing_Asttypes
module Parsetree = LintParsing_Parsetree
module Parse = LintParsing_Parse
module Ast_iterator = LintParsing_Ast_iterator

module StringMap = Map.Make(String)
			
module MakeArg = struct

  open Asttypes
  open Parsetree
  open Longident
	     
  let enter_class_declaration cls =
    Some ("class", cls.pci_expr.pcl_loc) (****A CHANGER****)

  let enter_expression exp =
    match exp.pexp_desc with
    | Pexp_try _ -> Some ("pexp_try", exp.pexp_loc)
    | Pexp_match _ -> Some ("pexp_match",exp.pexp_loc)
    | Pexp_while _ -> Some ("pexp_while", exp.pexp_loc)
    | Pexp_for _ -> Some ("pexp_for", exp.pexp_loc)
    | Pexp_ifthenelse (_,_,Some _) -> Some ("pexp_ifthenelse", exp.pexp_loc)
    | Pexp_ifthenelse (_,_,None) -> Some ("pexp_ifthen", exp.pexp_loc)
    | Pexp_new _ -> Some ("pexp_new", exp.pexp_loc)
    | Pexp_send _ -> Some ("pexp_send", exp.pexp_loc)
    | Pexp_open _ -> Some ("pexp_open", exp.pexp_loc)
    | Pexp_letexception _ -> Some("pexp_letexception", exp.pexp_loc)
    | Pexp_extension _ -> Some("pexp_extension", exp.pexp_loc)
    | _ -> None

  let enter_pattern pat =
    match pat.ppat_desc with
    | Ppat_open _ -> Some("ppat_open", pat.ppat_loc)
    | Ppat_exception _ -> Some ("ppat_exception", pat.ppat_loc)
    | Ppat_unpack _ -> Some("ppat_unpack", pat.ppat_loc)
    | Ppat_extension _ -> Some("ppat_extension", pat.ppat_loc)	   
    | _ -> None

  let enter_type typ =
    match typ.ptype_kind with
    | Ptype_open -> Some ("ptype_open", typ.ptype_loc)
    | _ -> None

  let enter_module_type mtyp =
    match mtyp.pmty_desc with
    | Pmty_alias _ -> Some("pmty_alias", mtyp.pmty_loc)
    | Pmty_extension _ -> Some("pmty_extension", mtyp.pmty_loc)
    | _ -> None
	     
  let main source =
    let ic = open_in source in
    Location.input_name := source;
    let lexbuf = Lexing.from_channel ic in
    Location.init lexbuf source;
    let str =
      try
        LintParsing_Parse.implementation lexbuf
      with exn ->
        close_in ic;
        raise exn
    in
    let open Ast_iterator in
    let features_map = ref (StringMap.empty) in
    let add_feature = function
      | Some(ft,loc) ->
         let mapping =
	   try
	     loc :: StringMap.find ft !features_map
	   with
	     Not_found -> [loc]
	 in
	 features_map := StringMap.add ft mapping !features_map
      | None -> ()
    in
    let this_iterator =
      { default_iterator with
        class_declaration =
          (fun iterator cls ->
             add_feature (enter_class_declaration cls);
             default_iterator.class_declaration iterator cls);
	expr =
	  (fun iterator exp ->
	     add_feature (enter_expression exp);
	     default_iterator.expr iterator exp);
	pat =
	  (fun iterator pat ->
	     add_feature (enter_pattern pat);
	     default_iterator.pat iterator pat);
        type_declaration =
	  (fun iterator typ ->
	     add_feature (enter_type typ);
	     default_iterator.type_declaration iterator typ);
	module_type =
	  (fun iterator mtyp ->
	     add_feature (enter_module_type mtyp);
	     default_iterator.module_type iterator mtyp);
      }
    in
    default_iterator.structure this_iterator str;
    let err_of_feature = function 
      | "class" -> UseClass (**** CHANGER NOM ****)
      | "pexp_try" -> UseTryWith
      | "pexp_match" -> UseMatch
      | "ppat_exception" -> UseExceptionPat
      | "pexp_while" -> UseWhile
      | "pexp_for" -> UseFor
      | "pexp_ifthenelse" -> UseIfThenElse
      | "pexp_ifthen" -> UseIfThen
      | "pexp_new" -> UseClass
      | "pexp_send" -> UseMethod
      | "pexp_open" -> UseLocalOpen
      | "ppat_open" -> UseLocalOpen
      | "pexp_letexception" -> UseLocalException
      | "ppat_unpack" -> UseUnpack
      | "pmty_alias"
      | "pexp_extension"
      | "ppat_extension"
      | "pmty_extension"
      | _ -> failwith "unknow feature"
    in
    StringMap.iter begin fun k v ->
      let err = err_of_feature k in
      List.iter (fun loc -> Warnings.report loc err) v
    end !features_map

end

module Main = Linter.MakeInputML(MakeArg)

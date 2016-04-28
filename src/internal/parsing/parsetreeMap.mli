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


open Parsetree

module type MapArgument = sig

#if OCAML_VERSION = "4.01.0+ocp1"

  val enter_exception_declaration :
    exception_declaration -> exception_declaration
  val enter_modtype_declaration : modtype_declaration -> modtype_declaration
  val leave_exception_declaration :
    exception_declaration -> exception_declaration
  val leave_modtype_declaration : modtype_declaration -> modtype_declaration
  val enter_core_field_type : core_field_type -> core_field_type
  val leave_core_field_type : core_field_type -> core_field_type

#else

  val enter_case : case -> case
  val leave_case : case -> case
  val enter_module_type_declaration : module_type_declaration -> module_type_declaration
  val leave_module_type_declaration : module_type_declaration -> module_type_declaration
  val enter_module_declaration : module_declaration -> module_declaration
  val leave_module_declaration : module_declaration -> module_declaration
  val enter_constructor_declaration : constructor_declaration -> constructor_declaration
  val leave_constructor_declaration : constructor_declaration -> constructor_declaration
  val enter_label_declaration : label_declaration -> label_declaration
  val leave_label_declaration : label_declaration -> label_declaration
  val enter_module_binding : module_binding -> module_binding
  val leave_module_binding : module_binding -> module_binding
  val enter_value_binding : value_binding -> value_binding
  val leave_value_binding : value_binding -> value_binding
  val enter_open_description : open_description -> open_description
  val leave_open_description : open_description -> open_description

  val enter_type_extension : type_extension -> type_extension
  val leave_type_extension : type_extension -> type_extension

  val enter_extension_constructor : extension_constructor -> extension_constructor
  val leave_extension_constructor : extension_constructor -> extension_constructor
#endif

  val enter_structure : structure -> structure
  val enter_value_description : value_description -> value_description
  val enter_type_declaration : type_declaration -> type_declaration
  val enter_pattern : pattern -> pattern
  val enter_expression : expression -> expression
  val enter_package_type : package_type -> package_type
  val enter_signature : signature -> signature
  val enter_signature_item : signature_item -> signature_item
  val enter_module_type : module_type -> module_type
  val enter_module_expr : module_expr -> module_expr
  val enter_with_constraint : with_constraint -> with_constraint
  val enter_class_expr : class_expr -> class_expr
  val enter_class_signature : class_signature -> class_signature
  val enter_class_description : class_description -> class_description
  val enter_class_type_declaration :
    class_type_declaration -> class_type_declaration
  val enter_class_infos : 'a class_infos -> 'a class_infos
  val enter_class_type : class_type -> class_type
  val enter_core_type : core_type -> core_type
  val enter_class_structure : class_structure -> class_structure
  val enter_class_field : class_field -> class_field
  val enter_structure_item : structure_item -> structure_item
  val leave_structure : structure -> structure
  val leave_value_description : value_description -> value_description
  val leave_type_declaration : type_declaration -> type_declaration
  val leave_pattern : pattern -> pattern
  val leave_expression : expression -> expression
  val leave_package_type : package_type -> package_type
  val leave_signature : signature -> signature
  val leave_signature_item : signature_item -> signature_item
  val leave_module_type : module_type -> module_type
  val leave_module_expr : module_expr -> module_expr
  val leave_with_constraint : with_constraint -> with_constraint
  val leave_class_expr : class_expr -> class_expr
  val leave_class_signature : class_signature -> class_signature
  val leave_class_description : class_description -> class_description
  val leave_class_type_declaration :
    class_type_declaration -> class_type_declaration
  val leave_class_infos : 'a class_infos -> 'a class_infos
  val leave_class_type : class_type -> class_type
  val leave_core_type : core_type -> core_type
  val leave_class_structure : class_structure -> class_structure
  val leave_class_field : class_field -> class_field
  val leave_structure_item : structure_item -> structure_item
  val enter_class_type_field : class_type_field -> class_type_field
  val leave_class_type_field : class_type_field -> class_type_field
end


module MakeMap(Map : MapArgument) : sig
  val map_structure : Parsetree.structure -> Parsetree.structure
  val map_signature : Parsetree.signature -> Parsetree.signature
end

module DefaultMapArgument : MapArgument

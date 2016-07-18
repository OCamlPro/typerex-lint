val typ_collector : (string * Parsetree.core_type) list ref -> Ast_mapper.mapper

val collect : Parsetree.type_declaration list -> Parsetree.type_declaration list

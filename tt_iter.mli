class iterator :
object
  method clear_type_table : unit
  method class_declaration : Typedtree.class_declaration -> unit
  method class_description : Typedtree.class_description -> unit
  method class_expr : Typedtree.class_expr -> unit
  method class_expr_desc : Typedtree.class_expr_desc -> unit
  method class_field : Typedtree.class_field -> unit
  method class_field_desc : Typedtree.class_field_desc -> unit
  method class_field_kind : Typedtree.class_field_kind -> unit
  method class_signature : Typedtree.class_signature -> unit
  method class_structure : Typedtree.class_structure -> unit
  method class_type : Typedtree.class_type -> unit
  method class_type_declaration : Typedtree.class_type_declaration -> unit
  method class_type_desc : Typedtree.class_type_desc -> unit
  method class_type_field : Typedtree.class_type_field -> unit
  method class_type_field_desc : Typedtree.class_type_field_desc -> unit
  method constructor_description : Types.constructor_description -> unit
  method core_field_desc : Typedtree.core_field_desc -> unit
  method core_field_type : Typedtree.core_field_type -> unit
  method core_type : Typedtree.core_type -> unit
  method ctypdesc : Typedtree.core_type_desc -> unit
  method env : Env.t -> unit
  method exception_declaration : Typedtree.exception_declaration -> unit
  method exp_extra : Typedtree.exp_extra -> unit
  method expression : Typedtree.expression -> unit
  method expression_desc : Typedtree.expression_desc -> unit
  method ident : Ident.t -> unit
  method label_description : Types.label_description -> unit
  method location : Location.t -> unit
  method longident : Longident.t -> unit
  method longident_loc : Longident.t Asttypes.loc -> unit
  method meth : Typedtree.meth -> unit
  method modtype_declaration : Typedtree.modtype_declaration -> unit
  method module_coercion : Typedtree.module_coercion -> unit
  method module_expr : Typedtree.module_expr -> unit
  method module_expr_desc : Typedtree.module_expr_desc -> unit
  method module_type : Typedtree.module_type -> unit
  method module_type_constraint : Typedtree.module_type_constraint -> unit
  method module_type_desc : Typedtree.module_type_desc -> unit
  method package_type : Typedtree.package_type -> unit
  method pat_extra : Typedtree.pat_extra -> unit
  method path : Path.t -> unit
  method pattern : Typedtree.pattern -> unit
  method pattern_desc : Typedtree.pattern_desc -> unit
  method row_field : Typedtree.row_field -> unit
  method signature : Typedtree.signature -> unit
  method signature_item : Typedtree.signature_item -> unit
  method signature_item_desc : Typedtree.signature_item_desc -> unit
  method string_loc : string Asttypes.loc -> unit
  method structure : Typedtree.structure -> unit
  method structure_item : Typedtree.structure_item -> unit
  method structure_item_desc : Typedtree.structure_item_desc -> unit
  method type_declaration : Typedtree.type_declaration -> unit
  method type_kind : Typedtree.type_kind -> unit
  method value_description : Typedtree.value_description -> unit
  method with_constraint : Typedtree.with_constraint -> unit

    (* itering on Types.* start with t_ to avoid collision *)
  method t_abbrev_memo : Types.abbrev_memo -> unit
  method t_class_declaration : Types.class_declaration -> unit
  method t_class_signature : Types.class_signature -> unit
  method t_class_type : Types.class_type -> unit
  method t_class_type_declaration : Types.class_type_declaration -> unit
  method t_exception_declaration : Types.exception_declaration -> unit
  method t_modtype_declaration : Types.modtype_declaration -> unit
  method t_module_type : Types.module_type -> unit
  method t_row_desc : Types.row_desc -> unit
  method t_row_field : Types.row_field -> unit
  method t_signature : Types.signature -> unit
  method t_signature_item : Types.signature_item -> unit
  method t_type_declaration : Types.type_declaration -> unit
  method t_type_desc : Types.type_desc -> unit
  method t_type_expr : Types.type_expr -> unit
  method t_type_kind : Types.type_kind -> unit
  method t_value_description : Types.value_description -> unit
  method t_value_kind : Types.value_kind -> unit

end

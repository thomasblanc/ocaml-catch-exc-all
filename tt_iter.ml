open Typedtree
open Types

let iter_ref f r = f !r
let iter_option f = function Some x -> f x | None -> ()
let iter_meths f = Meths.iter (fun _ x -> f x)
let iter f l = List.iter f l
let iter_tuple f g (a,b) = f a; g b
let iter2 f g = iter (iter_tuple f g)
let iter3 f g h = iter (fun (a,b,c)-> f a; g b; h c)
let iter4 f g h i = iter (fun (a,b,c,d)-> f a; g b; h c;i d)
let iter5 f g h i j = iter (fun (a,b,c,d,e)-> f a; g b; h c; i d; j e)

class iterator =
object (self)

  val te_table = Hashtbl.create 100

  method clear_type_table = Hashtbl.reset te_table

  method pattern pat =
    self#pattern_desc pat.pat_desc;
    self#location pat.pat_loc;
    iter2 self#pat_extra self#location pat.pat_extra;
    self#t_type_expr pat.pat_type;
    self#env pat.pat_env

  method pat_extra =
    function
    | Tpat_constraint (ctyp ) ->
      self#core_type ctyp
    | Tpat_type ( (p, loc) ) ->
      self#path p;
      self#longident_loc loc
    | Tpat_unpack -> ()

  method pattern_desc =
    function
    | Tpat_any -> ()
    | Tpat_var ( i, sloc ) ->
      self#ident i;
      self#string_loc sloc
    | Tpat_alias ( pat, i, sloc ) ->
      self#pattern pat;
      self#ident i;
      self#string_loc sloc
    | Tpat_constant _ -> ()
    | Tpat_tuple ( l ) -> iter self#pattern l
    | Tpat_construct ( loc, cdescr, l, b ) ->
      self#longident_loc loc;
      self#constructor_description cdescr;
      iter self#pattern l
    | Tpat_variant ( label, pat, rdesc ) ->
      iter_option self#pattern pat;
      iter_ref self#t_row_desc rdesc
    | Tpat_record ( l, flag ) ->
      iter3 self#longident_loc self#label_description self#pattern l;
    | Tpat_array ( l ) ->
      iter self#pattern l
    | Tpat_or ( pat, pat2, row_desc ) ->
      self#pattern pat;
      self#pattern pat2;
      iter_option self#t_row_desc row_desc
    | Tpat_lazy ( pat ) ->
      self#pattern pat

  method expression exp =
    self#expression_desc exp.exp_desc;
    self#location exp.exp_loc;
    iter2 self#exp_extra self#location exp.exp_extra;
    self#t_type_expr exp.exp_type;
    self#env exp.exp_env

  method exp_extra =
    function
    | Texp_constraint ( ctyp, ctyp2 ) ->
      iter_option self#core_type ctyp;
      iter_option self#core_type ctyp2
    | Texp_open ( flag, path, loc, env ) ->
      self#path path;
      self#longident_loc loc;
      self#env env
    | Texp_poly ( ctyp ) ->
      iter_option self#core_type ctyp
    | Texp_newtype _ -> ()

  method expression_desc =
    function
    | Texp_ident ( p, loc, vd ) ->
      self#path p;
      self#longident_loc loc;
      self#t_value_description vd
    | Texp_constant _ -> ()
    | Texp_let ( flag, l, expr ) ->
      iter2 self#pattern self#expression l;
      self#expression expr
    | Texp_function ( label, l, partial ) ->
      iter2 self#pattern self#expression l
    | Texp_apply ( expr, l ) ->
      self#expression expr;
      iter3 ignore (iter_option self#expression) ignore l
    | Texp_match ( expr, l, _ ) ->
      self#expression expr;
      iter2 self#pattern self#expression l
    | Texp_try ( expr, l ) ->
      self#expression expr;
      iter2 self#pattern self#expression l
    | Texp_tuple ( l ) ->
      iter self#expression l
    | Texp_construct ( loc, cdescr, l, _ ) ->
      self#longident_loc loc;
      self#constructor_description cdescr;
      iter self#expression l
    | Texp_variant ( _, expr ) ->
      iter_option self#expression expr
    | Texp_record ( l, expr ) ->
      iter3 self#longident_loc self#label_description self#expression l;
      iter_option self#expression expr
    | Texp_field ( expr, loc, ldescr ) ->
      self#expression expr;
      self#longident_loc loc;
      self#label_description ldescr
    | Texp_setfield ( expr, loc, ldescr, expr2 ) ->
      self#expression expr;
      self#longident_loc loc;
      self#label_description ldescr;
      self#expression expr2
    | Texp_array ( l ) ->
      iter self#expression l
    | Texp_ifthenelse ( expr, expr2, expr3 ) ->
      self#expression expr;
      self#expression expr2;
      iter_option self#expression expr3
    | Texp_sequence ( expr, expr2 ) ->
      self#expression expr;
      self#expression expr2
    | Texp_while ( expr, expr2 ) ->
      self#expression expr;
      self#expression expr2
    | Texp_for ( i, sloc, expr, expr2, _, expr3 ) ->
      self#ident i;
      self#string_loc sloc;
      self#expression expr;
      self#expression expr2;
      self#expression expr3
    | Texp_when ( expr, expr2 ) ->
      self#expression expr;
      self#expression expr2
    | Texp_send ( expr, meth, expr2 ) ->
      self#expression expr;
      self#meth meth;
      iter_option self#expression expr2
    | Texp_new ( p, loc, cdecl ) ->
      self#path p;
      self#longident_loc loc;
      self#t_class_declaration cdecl
    | Texp_instvar ( p, p2, sloc ) ->
      self#path p;
      self#path p2;
      self#string_loc sloc
    | Texp_setinstvar ( p, p2, sloc, expr ) ->
      self#path p;
      self#path p2;
      self#string_loc sloc;
      self#expression expr
    | Texp_override ( p, l ) ->
      self#path p;
      iter3 self#path self#string_loc self#expression l
    | Texp_letmodule ( i, sloc, mexpr, expr ) ->
      self#ident i;
      self#string_loc sloc;
      self#module_expr mexpr;
      self#expression expr
    | Texp_assert ( expr ) ->
      self#expression expr
    | Texp_assertfalse -> ()
    | Texp_lazy ( expr ) ->
      self#expression expr
    | Texp_object ( cstr, _ ) ->
      self#class_structure cstr;
    | Texp_pack ( mexpr ) ->
      self#module_expr mexpr

  method meth =
    function
    | Tmeth_name _ -> ()
    | Tmeth_val ( i ) -> self#ident i


  method class_expr cl =
    self#class_expr_desc cl.cl_desc;
    self#location cl.cl_loc;
    self#t_class_type cl.cl_type;
    self#env cl.cl_env

  method class_expr_desc =
    function
    | Tcl_ident (p, loc, l ) ->
      self#path p;
      self#longident_loc loc;
      iter self#core_type l
    | Tcl_structure ( cstr ) -> self#class_structure cstr
    | Tcl_fun ( _, pat, l, cexpr, _ ) ->
      self#pattern pat;
      iter3 self#ident self#string_loc self#expression l;
      self#class_expr cexpr
    | Tcl_apply ( cexpr, l ) ->
      self#class_expr cexpr;
      iter3 ignore (iter_option self#expression) ignore l
    | Tcl_let ( _, l1, l2, cexpr ) ->
      iter2 self#pattern self#expression l1;
      iter3 self#ident self#string_loc self#expression l2;
      self#class_expr cexpr
    | Tcl_constraint ( cexpr, ctyp, _, _, _ ) ->
      self#class_expr cexpr;
      iter_option self#class_type ctyp

  method class_structure cstr =
    self#pattern cstr.cstr_pat;
    iter self#class_field cstr.cstr_fields;
    self#t_class_signature cstr.cstr_type;
    iter_meths self#ident cstr.cstr_meths

  method class_field cf =
    self#class_field_desc cf.cf_desc;
    self#location cf.cf_loc

  method class_field_kind =
    function
    | Tcfk_virtual ( ctyp ) -> self#core_type ctyp
    | Tcfk_concrete ( expr ) -> self#expression expr

  method class_field_desc =
    function
    | Tcf_inher ( _, cexpr, _, l1, l2 ) ->
      self#class_expr cexpr;
      iter2 ignore self#ident l1;
      iter2 ignore self#ident l2
    | Tcf_val ( _, sloc, _, i, cf_kind, _ ) ->
      self#string_loc sloc;
      self#ident i;
      self#class_field_kind cf_kind
    | Tcf_meth ( _, sloc, _, cf_kind, _ ) ->
      self#string_loc sloc;
      self#class_field_kind cf_kind
    | Tcf_constr ( ctyp, ctyp2 ) ->
      self#core_type ctyp;
      self#core_type ctyp2
    | Tcf_init ( expr ) -> self#expression expr


  method module_expr mod_e =
    self#module_expr_desc mod_e.mod_desc;
    self#location mod_e.mod_loc;
    self#t_module_type mod_e.mod_type;
    self#env mod_e.mod_env

  method module_type_constraint =
    function
    | Tmodtype_implicit -> ()
    | Tmodtype_explicit ( mtyp ) -> self#module_type mtyp

  method module_expr_desc =
    function
    | Tmod_ident ( p, loc ) ->
      self#path p;
      self#longident_loc loc
    | Tmod_structure ( str ) -> self#structure str
    | Tmod_functor ( i, sloc, mtyp, mexpr ) ->
      self#ident i;
      self#string_loc sloc;
      self#module_type mtyp;
      self#module_expr mexpr
    | Tmod_apply ( mexpr, mexpr2, mcoerce ) ->
      self#module_expr mexpr;
      self#module_expr mexpr2;
      self#module_coercion mcoerce
    | Tmod_constraint ( mexpr, mtyp, mconstr, mcoerce ) ->
      self#module_expr mexpr;
      self#t_module_type mtyp;
      self#module_type_constraint mconstr;
      self#module_coercion mcoerce
    | Tmod_unpack ( expr, mtyp ) ->
      self#expression expr;
      self#t_module_type mtyp

  method structure str =
    iter self#structure_item str.str_items;
    self#t_signature str.str_type;
    self#env str.str_final_env

  method structure_item str =
    self#structure_item_desc str.str_desc;
    self#location str.str_loc;
    self#env str.str_env

  method structure_item_desc =
    function
    | Tstr_eval (expr ) -> self#expression expr
    | Tstr_value ( _, l ) -> iter2 self#pattern self#expression l
    | Tstr_primitive ( i, sloc, vdescr ) ->
      self#ident i;
      self#string_loc sloc;
      self#value_description vdescr
    | Tstr_type ( l ) -> iter3 self#ident self#string_loc self#type_declaration l
    | Tstr_exception ( i, sloc, exn_decl ) ->
      self#ident i;
      self#string_loc sloc;
      self#exception_declaration exn_decl
    | Tstr_exn_rebind ( i, sloc, p, loc ) ->
      self#ident i;
      self#string_loc sloc;
      self#path p;
      self#longident_loc loc
    | Tstr_module ( i, sloc, mexpr ) ->
      self#ident i;
      self#string_loc sloc;
      self#module_expr mexpr
    | Tstr_recmodule ( l ) -> iter4 self#ident self#string_loc self#module_type self#module_expr l
    | Tstr_modtype ( i, sloc, mtyp ) ->
      self#ident i;
      self#string_loc sloc;
      self#module_type mtyp
    | Tstr_open ( _, path, loc) ->
      self#path path;
      self#longident_loc loc
    | Tstr_class ( l ) -> iter3 self#class_declaration ignore ignore l
    | Tstr_class_type ( l ) -> iter3 self#ident self#string_loc self#class_type_declaration l
    | Tstr_include ( mexpr, l ) ->
      self#module_expr mexpr;
      iter self#ident l

  method module_coercion =
    function
    | Tcoerce_none -> ()
    | Tcoerce_structure ( l ) -> iter2 ignore self#module_coercion l
    | Tcoerce_functor ( mcoerce, mcoerce2 ) ->
      self#module_coercion mcoerce;
      self#module_coercion mcoerce2
    | Tcoerce_primitive _ -> ()

  method module_type mty =
    self#module_type_desc mty.mty_desc;
    self#t_module_type mty.mty_type;
    self#env mty.mty_env;
    self#location mty.mty_loc

  method module_type_desc =
    function
    | Tmty_ident ( p, loc ) ->
      self#path p;
      self#longident_loc loc
    | Tmty_signature ( sign ) -> self#signature sign
    | Tmty_functor ( i, sloc, mtyp, mtyp2 ) ->
      self#ident i;
      self#string_loc sloc;
      self#module_type mtyp;
      self#module_type mtyp2
    | Tmty_with ( mtyp, l ) ->
      self#module_type mtyp;
      iter3 self#path self#longident_loc self#with_constraint l
    | Tmty_typeof ( mexpr ) -> self#module_expr mexpr

  method signature sign =
    iter self#signature_item sign.sig_items;
    self#t_signature sign.sig_type;
    self#env sign.sig_final_env

  method signature_item sign =
    self#signature_item_desc sign.sig_desc;
    self#env sign.sig_env;
    self#location sign.sig_loc

  method signature_item_desc =
    function
    | Tsig_value ( i, sloc, vdescr ) ->
      self#ident i;
      self#string_loc sloc;
      self#value_description vdescr
    | Tsig_type ( l ) -> iter3 self#ident self#string_loc self#type_declaration l
    | Tsig_exception ( i, sloc, exn_decl ) ->
      self#ident i;
      self#string_loc sloc;
      self#exception_declaration exn_decl
    | Tsig_module ( i, sloc, mtyp ) ->
      self#ident i;
      self#string_loc sloc;
      self#module_type mtyp
    | Tsig_recmodule ( l ) -> iter3 self#ident self#string_loc self#module_type l
    | Tsig_modtype ( i, sloc, mtyp_decl ) ->
      self#ident i;
      self#string_loc sloc;
      self#modtype_declaration mtyp_decl
    | Tsig_open ( _, path, loc ) ->
      self#path path;
      self#longident_loc loc
    | Tsig_include ( mtyp, sign ) ->
      self#module_type mtyp;
      self#t_signature sign
    | Tsig_class ( l ) -> iter self#class_description l
    | Tsig_class_type ( l ) -> iter self#class_type_declaration l

  method modtype_declaration =
    function
    | Tmodtype_abstract -> ()
    | Tmodtype_manifest ( mtyp ) -> self#module_type mtyp

  method with_constraint =
    function
    | Twith_type ( typ_decl )
    | Twith_typesubst ( typ_decl ) -> self#type_declaration typ_decl
    | Twith_module ( p, loc )
    | Twith_modsubst ( p, loc ) ->
      self#path p;
      self#longident_loc loc

  method core_type ctyp =
    self#ctypdesc ctyp.ctyp_desc;
    self#t_type_expr ctyp.ctyp_type;
    self#env ctyp.ctyp_env;
    self#location ctyp.ctyp_loc

  method ctypdesc =
    function
    | Ttyp_any
    | Ttyp_var _ -> ()
    | Ttyp_arrow ( _, ctyp, ctyp2 ) ->
      self#core_type ctyp;
      self#core_type ctyp2
    | Ttyp_tuple ( l ) -> iter self#core_type l
    | Ttyp_constr ( p, loc, l ) ->
      self#path p;
      self#longident_loc loc;
      iter self#core_type l
    | Ttyp_object ( l ) -> iter self#core_field_type l
    | Ttyp_class ( p, loc, l1, _ ) ->
      self#path p;
      self#longident_loc loc;
      iter self#core_type l1
    | Ttyp_alias ( ctyp, _ )
    | Ttyp_poly ( _, ctyp ) -> self#core_type ctyp
    | Ttyp_variant ( l1, _, _ ) -> iter self#row_field l1
    | Ttyp_package ( pack ) -> self#package_type pack

  method package_type pack =
    self#path pack.pack_name;
    iter2 self#longident_loc self#core_type pack.pack_fields;
    self#t_module_type pack.pack_type;
    self#longident_loc pack.pack_txt

  method core_field_type field =
    self#core_field_desc field.field_desc;
    self#location field.field_loc

  method core_field_desc =
    function
    | Tcfield ( _, ctyp ) -> self#core_type ctyp
    | Tcfield_var -> ()

  method row_field =
    function
    | Ttag ( _, _, l ) -> iter self#core_type l
    | Tinherit ( ctyp ) -> self#core_type ctyp

  method value_description val_d =
    self#core_type val_d.val_desc;
    self#t_value_description val_d.val_val;
    iter ignore val_d.val_prim;
    self#location val_d.val_loc

  method type_declaration typ =
    self#t_type_declaration typ.typ_type;
    iter3 self#core_type self#core_type self#location typ.typ_cstrs;
    self#type_kind typ.typ_kind;
    iter_option self#core_type typ.typ_manifest;
    self#location typ.typ_loc

  method type_kind =
    function
    | Ttype_abstract -> ()
    | Ttype_variant ( l ) -> iter4 self#ident self#string_loc (iter self#core_type) self#location l
    | Ttype_record ( l ) -> iter5 self#ident self#string_loc ignore self#core_type self#location l

  method exception_declaration exn_d =
    iter self#core_type exn_d.exn_params;
    self#t_exception_declaration exn_d.exn_exn;
    self#location exn_d.exn_loc

  method class_type cltyp =
    self#class_type_desc cltyp.cltyp_desc;
    self#t_class_type cltyp.cltyp_type;
    self#env cltyp.cltyp_env;
    self#location cltyp.cltyp_loc

  method class_type_desc =
    function
    | Tcty_constr ( p, loc, l ) ->
      self#path p;
      self#longident_loc loc;
      iter self#core_type l
    | Tcty_signature ( csign ) -> self#class_signature csign
    | Tcty_fun ( label, ctyp, cltyp ) ->
      self#core_type ctyp;
      self#class_type cltyp

  method class_signature csig =
    self#core_type csig.csig_self;
    iter self#class_type_field csig.csig_fields;
    self#t_class_signature csig.csig_type;
    self#location csig.csig_loc

  method class_type_field ctf =
    self#class_type_field_desc ctf.ctf_desc;
    self#location ctf.ctf_loc

  method class_type_field_desc =
    function
    | Tctf_inher (cltyp ) -> self#class_type cltyp
    | Tctf_val ( _, _, _, ctyp )
    | Tctf_virt ( _, _, ctyp)
    | Tctf_meth ( _, _, ctyp ) -> self#core_type ctyp
    | Tctf_cstr ( ctyp, ctyp2 ) ->
      self#core_type ctyp;
      self#core_type ctyp2

  method class_declaration cdecl =
    iter self#string_loc (fst cdecl.ci_params);
    self#location (snd cdecl.ci_params);
    self#string_loc cdecl.ci_id_name;
    self#ident cdecl.ci_id_class;
    self#ident cdecl.ci_id_class_type;
    self#ident cdecl.ci_id_object;
    self#ident cdecl.ci_id_typesharp;
    self#class_expr cdecl.ci_expr;
    self#t_class_declaration cdecl.ci_decl;
    self#t_class_type_declaration cdecl.ci_type_decl;
    self#location cdecl.ci_loc

  method class_description cdescr =
    iter self#string_loc (fst cdescr.ci_params);
    self#location (snd cdescr.ci_params);
    self#string_loc cdescr.ci_id_name;
    self#ident cdescr.ci_id_class;
    self#ident cdescr.ci_id_class_type;
    self#ident cdescr.ci_id_object;
    self#ident cdescr.ci_id_typesharp;
    self#class_type cdescr.ci_expr;
    self#t_class_declaration cdescr.ci_decl;
    self#t_class_type_declaration cdescr.ci_type_decl;
    self#location cdescr.ci_loc

  method class_type_declaration ctdecl =
    iter self#string_loc (fst ctdecl.ci_params);
    self#location (snd ctdecl.ci_params);
    self#string_loc ctdecl.ci_id_name;
    self#ident ctdecl.ci_id_class;
    self#ident ctdecl.ci_id_class_type;
    self#ident ctdecl.ci_id_object;
    self#ident ctdecl.ci_id_typesharp;
    self#class_type ctdecl.ci_expr;
    self#t_class_declaration ctdecl.ci_decl;
    self#t_class_type_declaration ctdecl.ci_type_decl;
    self#location ctdecl.ci_loc


  (* itering on Types.* start with t_ to avoid collision *)
  method t_value_description vd =
    self#t_type_expr vd.val_type;
    self#t_value_kind vd.val_kind;
    self#location vd.val_loc
  method t_type_declaration td =
    iter self#t_type_expr td.type_params;
    self#t_type_kind td.type_kind;
    iter_option self#t_type_expr td.type_manifest;
    self#location td.type_loc
  method t_signature = iter self#t_signature_item
  method t_signature_item = function
  | Sig_value ( i , vd ) ->
    self#ident i;
    self#t_value_description vd
  | Sig_type ( i , td , r ) ->
    self#ident i;
    self#t_type_declaration td;
  | Sig_exception ( i , exnd ) ->
    self#ident i;
    self#t_exception_declaration exnd
  | Sig_module ( i , mt , _ ) ->
    self#ident i;
    self#t_module_type mt
  | Sig_modtype ( i , md ) ->
    self#ident i;
    self#t_modtype_declaration md
  | Sig_class ( i , cd , _ ) ->
    self#ident i;
    self#t_class_declaration cd
  | Sig_class_type ( i , ctd , _ ) ->
    self#ident i;
    self#t_class_type_declaration ctd

  method t_module_type = function
  | Mty_ident p -> self#path p
  | Mty_signature s -> self#t_signature s
  | Mty_functor ( i , mtyp , mtyp2) ->
    self#ident i;
    self#t_module_type mtyp;
    self#t_module_type mtyp2

  method t_exception_declaration ed =
    iter self#t_type_expr ed.exn_args;
    self#location ed.exn_loc

  method t_class_type_declaration ctd =
    iter self#t_type_expr ctd.clty_params;
    self#t_class_type ctd.clty_type;
    self#path ctd.clty_path

  method t_class_type = function
  | Cty_constr ( p, l, ct ) ->
    self#path p;
    iter self#t_type_expr l;
    self#t_class_type ct
  | Cty_signature ( s ) -> self#t_class_signature s
  | Cty_fun ( _, typ , ctyp ) ->
    self#t_type_expr typ ;
    self#t_class_type ctyp

  method t_class_signature sgn =
    self#t_type_expr sgn.cty_self;
    iter2 self#path (iter self#t_type_expr) sgn.cty_inher (* shouldn't we iterate through vars ? *)

  method t_class_declaration cd =
    iter self#t_type_expr cd.cty_params;
    self#t_class_type cd.cty_type;
    self#path cd.cty_path;
    iter_option self#t_type_expr cd.cty_new

  method t_value_kind = function
  | Val_self ( meths, vars, s, expr ) ->
    iter_ref ( iter_meths ( iter_tuple self#ident  self#t_type_expr )) meths;
    iter_ref ( Vars.iter (fun _ (i,_,_,e) -> (self#ident i; self#t_type_expr e))) vars;
    self#t_type_expr expr
  | Val_anc ( l, _) -> iter2 ignore self#ident l
  | _ -> ()

  method t_type_kind = function
  | Type_abstract -> ()
  | Type_record ( l, repr ) -> iter3 self#ident ignore self#t_type_expr l;
  | Type_variant l -> iter3 self#ident ( iter self#t_type_expr ) ( iter_option self#t_type_expr ) l

  method t_type_expr expr =
    if Hashtbl.mem te_table expr.id
    then ()
    else
      begin
	Hashtbl.add te_table expr.id ();
	self#t_type_desc expr.desc
      end
	
  method t_type_desc = function
  | Tarrow ( _, e1, e2, _ ) ->
    self#t_type_expr e1;
    self#t_type_expr e2
  | Ttuple ( l ) -> iter self#t_type_expr l
  | Tconstr ( p, l, memo ) ->
    self#path p;
    iter self#t_type_expr l;
    iter_ref self#t_abbrev_memo memo
  | Tobject ( e, o ) ->
    self#t_type_expr e;
    iter_ref ( iter_option ( iter_tuple self#path ( iter self#t_type_expr))) o
  | Tfield ( _, _, e1, e2 ) ->
    self#t_type_expr e1;
    self#t_type_expr e2
  | Tlink ( e )
  | Tsubst ( e ) -> self#t_type_expr e
  | Tvariant ( r ) -> self#t_row_desc r
  | Tpoly ( e,l ) ->
    self#t_type_expr e;
    iter self#t_type_expr l
  | Tpackage ( p, l, exprs ) ->
    self#path p;
    iter self#longident l;
    iter self#t_type_expr exprs
  | d -> ()

  method t_modtype_declaration = function
  | Modtype_abstract -> ()
  | Modtype_manifest ( mt ) -> self#t_module_type mt

  method t_row_desc row =
    iter2 ignore self#t_row_field row.row_fields;
    self#t_type_expr row.row_more;
    iter_option (iter_tuple self#path  (iter self#t_type_expr)) row.row_name;
    
  method t_row_field = function
  | Rpresent ( e ) -> iter_option self#t_type_expr e
  | Reither ( _ ,l , _, field ) ->
    iter self#t_type_expr l;
    iter_ref (iter_option self#t_row_field) field
  | Rabsent -> ()

  method t_abbrev_memo = function
  | Mnil -> ()
  | Mcons ( _, p, e1, e2, m ) ->
    self#path p;
    self#t_type_expr e1;
    self#t_type_expr e2;
    self#t_abbrev_memo m
  | Mlink ( m ) ->
    iter_ref self#t_abbrev_memo m

  method longident _ = ()
  method string_loc _ = ()

  method path = let open Path in function
  | Pident i ->
    self#ident i
  | Pdot (p,s,i) ->
    self#path p;
  | Papply (p1,p2) ->
    self#path p1;
    self#path p2

  method longident_loc _ = ()
  method location _ = ()
  method label_description _ = ()
  method ident _ = ()
  method env _ = ()
  method constructor_description _ = ()

end

(* I found it simpler that way, yes it is *)

let demodule str = Translmod.trans_implementation "Main" ( str, Typedtree.Tcoerce_none)


(* old useless junk kept here just in case *)

(*open Ident
open Path
open Typedtree
open Helpers



class demodule last_id=
object (self)
(*  val mutable stamp = last_id *)
(*  val bindings = Hashtbl.create 100 : (int,Ident.t) Hashtbl.t *)
  inherit Identer.mapper last_id as super
(*  method mk_ident  =
    if Hashtbl.mem bindings id.stamp
    then
      Hashtbl.find bindings id.stamp
    else
      begin
	stamp <- succ stamp;
	let i = Ident.({stamp; name = s; flags = 0; }) in
	Hashtbl.add bindings id.stamp i;
	i
      end
*)
(*  method! path = function
  | Pdot (m, s, i) ->
    let m = self#path m in
    Pident (self#find_in_module m s)
  | Papply *) (* <--- insuffisant *)

  method! structure_item = function
  | {str_desc = Tstr_module ( id, mloc, mexpr); str_env = env; str_loc = loc} ->
    Tstr_value
      ( Asttypes.Nonrecursive,
	[{
	  pat_desc = Tpat_var (self#mk_ident id.name, mloc);
	  pat_loc = mloc;
	  pat_extra = [];
	  pat_type = self#modtype_to_tuple (mexpr.mod_type);
	  pat_env = env;
	},self#mod_to_tuple mexpr])
  | item -> super#structure_item_desc item

  method! mod_to_tuple = function
  | { mod_desc = d; mod_loc = loc; mod_type = t; mod_env = env; } ->
    let typ = self#modtype_to_tuple t in
    {
      exp_desc =
	begin
	  match d with
	  | Tmod_ident ( p, lloc) ->
	    let p = self#path p in
	    Texp_ident (p, lloc, typ)
	  | Tmod_structure s ->
	    let (exprs) = self#structure_to_exprs s in
	    Texp_tuple (exprs)
	  | Tmod_functor ( i, sloc, mtyp, mex) ->
	    Texp_function ( "", [(self#pattern_of_mtyp mtyp), self#mod_to_tuple mex], Total)
	  | Tmod_apply ( m1, m2, coerc) ->
	    let coercion =
	      {
		exp_desc = Texp_apply ( self#coercion_to_fun coerc, [("",self#mod_to_tuple m2,false)]);
		exp_loc = loc;
		exp_extra = [];
		exp_type = typ;
		exp_env = env
	      } in
	    Texp_apply (self#mod_to_tuple m1, ["",coercion,false])
	  | Tmod_constraint ( mex, mtyp, mconst, coerc) ->
	    
	  | Tmod_unpack ( e, mtyp) -> e.exp_desc
	end;
      exp_loc = loc;
      exp_extra = [];
      exp_type = typ;
      exp_env = env;
    }
    
  method structure_to_exprs s =
    let aux = function
      | { str_desc = d; str_loc = loc; str_env = env } :: tl ->
	begin
	  match d with
	      | Tstr_eval (expr ) ->
		let e = self#expression expr in
		let typ = e.exp_type in
		(mk_pat ~env ~typ Tpat_any, e) :: (self#structure_to_exprs tl)
	      | Tstr_value ( flag, l ) ->
		let l = map2 self#pattern self#expression l in
		l @ (self#structure_to_exprs tl)
	      | Tstr_module ( i, sloc, mexpr ) ->
		Tstr_module ( self#ident i, self#string_loc sloc, self#module_expr mexpr )
	      | Tstr_recmodule ( l ) ->
		Tstr_recmodule (map4 self#ident self#string_loc self#module_type self#module_expr l )
(*	      | Tstr_open ( flag, path, loc) ->
		Tstr_open ( flag, self#path path, self#longident_loc loc )*)
	      | Tstr_class ( l ) ->
		
		let rec my_map append f = function
		  | [] -> append
		  | (_,_,Virtual) :: tl -> my_map append f tl
		  | (cdecl, sl, _) :: tl -> ( f cdecl sl) :: ( my_map append f tl)
		in
		my_map
		  tl (fun cdecl sl ->
		    mk_expr
		      ~typ:()
		      ~env
		      (Texp_function
			 ("",[
			   mk_pat
			     ~typ:Helpers.typ_unit
			     ~env
			     (Tpat_construct (mknoloc "",
					      Helpers.cstr_unit,
					      [],false) ),
			   mk_expr ~typ:cdecl.ci_expr.cl_type ~env
			     (Texp_new (Pident cdecl.ci_id_class, mknoloc "", cdecl.ci_decl))
			 ])
		      )
		  ) l
		
		
		Tstr_class ( map3 self#class_declaration id id l )
	      | Tstr_include ( mexpr, l ) ->
		let l = seuoad in
		Tstr_include ( self#module_expr mexpr, map self#ident l)
	      | _ -> assert false
	end
      
      


    in aux s.str_items

end
*)

(*
   This file takes .cmt files on argv and produces one big merged cmt.
   There is an imprecise handling of envs (see typing/env.mli). But I won't use them anyway.
*)

open Typedtree
open Tt_mapper
open Cmt_format
open Types
open Ident

type definition_result = Cmt of Typedtree.structure_item | Cmi of Types.signature_item

module Im = Map.Make ( struct type t = int let compare = compare end )
module Sm = Map.Make ( struct type t = string let compare = compare end )
(* The actual code *)

exception Unspecified_module of string

let is_cmi s = s.[pred (String.length s)] = 'i'

class reident last includes =
object (self)
  inherit mapper as super
  val mutable x = last
  val mutable t = Im.empty
  val mutable import = includes
  val mutable last_env = Env.initial
  val env_cache =
    (Hashtbl.create 59 : ((Env.summary * Subst.t), Env.t) Hashtbl.t)

  method last_ident = x

  method reset_cache =
    Hashtbl.clear env_cache;
    Env.reset_cache()
  method private extract_sig env mty =
    match Mtype.scrape env mty with
      Mty_signature sg -> sg
    | _ -> assert false

  method add_module_env i m =
    (* Printf.printf "Adding %s with id %d\n%!" i.Ident.name i.Ident.stamp; *)
    last_env <- Env.add_module i m last_env
  method add_modtype_env i m =
    last_env <- Env.add_modtype i m last_env

  method! clear_type_table =
    t <- Im.empty;
    super#clear_type_table
  method! ident i = let s = i.stamp in
    if s > 1000
    then
      if Im.mem s t
      then { i with stamp = Im.find s t }
      else (  let a = self#fresh_ident in t <- Im.add s a t;  { i with stamp = a } )
    else
      if s = 0
      then { i with stamp = try Sm.find i.name import with Not_found -> 0 }
      else i

  method! env e = Env.env_of_only_summary self#env_from_summary e

  method fresh_ident = x <- succ x; x
  method add_import f =
    if not ( Sm.mem f import )
    then import <- Sm.add f self#fresh_ident import;
    Sm.find f import

  method env_from_summary sum subst = (* sans memoization, tout ca va exploser *)
    try
      Hashtbl.find env_cache (sum, subst)
    with Not_found ->
      let env =
	let open Env in
	match sum with (* tester si id < 1000 et mettre last_env *)
	| Env_value (_,id,_) | Env_type( _, id, _) | Env_exception(_, id, _) | Env_module(_, id, _) | Env_modtype(_, id, _) | Env_class(_, id, _) | Env_cltype (_, id, _)
	  when id.stamp < 1000 -> last_env
        | Env_empty -> last_env
	| Env_value(s, id, desc) ->
          Env.add_value (self#ident id) (Subst.value_description subst ( self#t_value_description desc)) (self#env_from_summary s subst)
	| Env_type(s, id, desc) ->
          Env.add_type (self#ident id) (Subst.type_declaration subst ( self#t_type_declaration desc)) (self#env_from_summary s subst)
	| Env_exception(s, id, desc) ->
          Env.add_exception (self#ident id) (Subst.exception_declaration subst ( self#t_exception_declaration desc)) (self#env_from_summary s subst)
	| Env_module(s, id, desc) ->
          Env.add_module (self#ident id) (Subst.modtype subst (self#t_module_type desc)) (self#env_from_summary s subst)
	| Env_modtype(s, id, desc) ->
          Env.add_modtype (self#ident id) (Subst.modtype_declaration subst (self#t_modtype_declaration desc)) (self#env_from_summary s subst)
	| Env_class(s, id, desc) ->
          Env.add_class (self#ident id) (Subst.class_declaration subst (self#t_class_declaration desc)) (self#env_from_summary s subst)
	| Env_cltype (s, id, desc) ->
          Env.add_cltype (self#ident id) (Subst.cltype_declaration subst (self#t_class_type_declaration desc)) (self#env_from_summary s subst)
	| Env_open(s, path) ->
          let env = self#env_from_summary s subst in
          let path' = Subst.module_path subst (self#path path) in
          let mty =
            try Env.find_module path' env with Not_found ->
	      (
		try
		  begin
		    match Env.find_modtype path' env with
		    | Modtype_manifest m -> m
		    | _ -> raise Not_found
		  end
		with Not_found ->
		  raise
		    (Unspecified_module
		       (match path' with
			 Path.Pident i -> Printf.sprintf "%d: %s" i.Ident.stamp i.Ident.name
		       | _ -> ""
		       )
		    )
	      )
          in
          Env.open_signature Asttypes.Override path' (self#extract_sig env mty) env
      in
      Hashtbl.add env_cache (sum, subst) env;
      env

  method ! path = function
  | p -> super#path p

end

let merge_cmts to_merge =
  let r = new reident 1000 Sm.empty in
  let initial_env = ref Env.empty
  and trees = ref [] in
  Array.iteri
    begin fun
      i fn -> 
	if is_cmi fn
	then
	  begin
	    let cmi = read_cmi fn in
	    let items = List.map r#t_signature_item cmi.Cmi_format.cmi_sign in
	    let mtype = Mty_signature items in
	    let id = r#add_import cmi.Cmi_format.cmi_name in
	    let i = Ident.({stamp = id; name = cmi.Cmi_format.cmi_name; flags = 0 }) in
	    r#add_modtype_env i (Modtype_manifest mtype)	    
	  end
	else
	  trees :=
	    begin
	      let cmt = read_cmt fn in
	      if i = 0 then initial_env := r#env cmt.cmt_initial_env;
	      ( match cmt.cmt_annots with
	      | Implementation s ->
		r#reset_cache;
		let s = r#structure s in
		r#clear_type_table;
		let mtype =
		  let cmi = try read_cmi fn with _ -> read_cmi (fn^"i") in
		  Mty_signature (cmi.Cmi_format.cmi_sign) in
		let id = r#add_import cmt.cmt_modname in
		let i = Ident.({stamp = id; name = cmt.cmt_modname; flags = 0 }) in
		r#add_module_env i mtype;
		{
		  str_desc =
		    Tstr_module (
		      i,
		      (mknoloc ""),
		      {
			mod_desc = Tmod_structure s;
			mod_loc = Location.none;
			mod_type = mtype;
			mod_env = cmt.cmt_initial_env;
		      }
		    );
		  str_loc = Location.none;
		  str_env = cmt.cmt_initial_env;
		}
	      | _ -> assert false
	      )
	    end :: !trees
    end to_merge;
  (
    {
      str_items = List.rev !trees;
      str_type =
	List.rev_map 
	  (function
	  | {str_desc= Tstr_module (i,_,{mod_type = m}) } -> Sig_module (i,m,Trec_not)
	  | _ -> assert false)
	  !trees;
      str_final_env = (match (List.hd !trees).str_desc with Tstr_module (_,_,{mod_desc = Tmod_structure s;_} ) -> s.str_final_env | _ -> assert false); (* a tester *)
    },
    r # last_ident
  )
  
(*
let () =
  let arg_last = pred (Array.length Sys.argv) in
  if arg_last <= 1
  then print_endline "please specify the cmts to load then the file you want to export"
  else
    begin
      let to_save = Sys.argv.(arg_last) in
      let r = new reident 1000 Sm.empty in
      (* a little hack in order to export *)
      Clflags.binary_annotations := true;
      Clflags.print_types := false;
      let initial_env = ref Env.empty
      and trees = ref [] in
      for i = 1 to pred arg_last do
	(* Printf.printf "In file %d: %s\n%!" i Sys.argv.(i); *)
	let fn = Sys.argv.(i) in
	  if is_cmi fn
	  then
	    begin
	      let cmi = read_cmi fn in
	      let items = List.map r#t_signature_item cmi.Cmi_format.cmi_sign in
	      let mtype = Mty_signature items in
	      let id = r#add_import cmi.Cmi_format.cmi_name in
	      let i = Ident.({stamp = id; name = cmi.Cmi_format.cmi_name; flags = 0 }) in
	      r#add_modtype_env i (Modtype_manifest mtype)	    
	    end
	  else
	    trees :=
	      begin
		let cmt = read_cmt fn in
		if i = 0 then initial_env := r#env cmt.cmt_initial_env;
		( match cmt.cmt_annots with
		| Implementation s ->
		  r#reset_cache;
		  let s = r#structure s in
		  r#clear_type_table;
		  let mtype =
		    let cmi = try read_cmi fn with _ -> read_cmi (fn^"i") in
		    Mty_signature (cmi.Cmi_format.cmi_sign) in
		  let id = r#add_import cmt.cmt_modname in
		  let i = Ident.({stamp = id; name = cmt.cmt_modname; flags = 0 }) in
		  r#add_module_env i mtype;
		  {
		    str_desc =
		      Tstr_module (
			i,
			(mknoloc ""),
			{
			  mod_desc = Tmod_structure s;
			  mod_loc = Location.none;
			  mod_type = mtype;
			mod_env = cmt.cmt_initial_env;
			}
		      );
		    str_loc = Location.none;
		    str_env = cmt.cmt_initial_env;
		  }
		| _ -> assert false
		)
	      end :: !trees
      done;

      save_cmt
	to_save
	"Exported"
	(Implementation
	   {
	     str_items = List.rev !trees;
	     str_type =
	       List.rev_map 
		 (function
		 | {str_desc= Tstr_module (i,_,{mod_type = m}) } -> Sig_module (i,m,Trec_not)
		 | _ -> assert false)
		 !trees;
	     str_final_env = (match (List.hd !trees).str_desc with Tstr_module (_,_,{mod_desc = Tmod_structure s;_} ) -> s.str_final_env | _ -> assert false); (* a tester *)
	   }
	)
	None
	!initial_env (* idem *)
	None
    end
*)

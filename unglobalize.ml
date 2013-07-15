open Lambda
open Ident

module Imap = Map.Make ( struct type t = Ident.t let compare a b = compare a.stamp b.stamp end )

let noarg = { stamp = 0; name = ""; flags = 0 }
let switch_ident = { stamp = max_int; name = "#switch_f"; flags = 0}
let apply_ident = { stamp = pred max_int; name = "#apply_f"; flags = 0}
let arg_ident = { stamp = pred ( pred max_int); name = "#arg_f"; flags = 0}

class transformer =
object (self)

  val mutable funs = (Imap.empty : lambda Imap.t)

  val mutable bigswitch =
    {
      sw_numconsts = 0;
      sw_consts = [];
      sw_numblocks = 0;
      sw_blocks = [];
      sw_failaction = None;
    }

  val mutable fv_to_change = IdentSet.empty
  val mutable fv_idents = []
  val mutable argument_var = noarg

  method private v2f v =
    let rec v2f_aux v r = function
      | [] -> raise Not_found
      | h :: t when h = v -> r
      | _ :: t -> v2f_aux v (succ r) t
    in
    v2f_aux v 1 fv_idents (* field 0 is the func ident *)

(*  method mk_construct l =
    counter <- succ counter;
    Lprim ( Pmakeblock (counter, Asttypes.Immutable), l) *)


  inherit Lmapper.mapper as super


  method! var i =
    try Imap.find i funs with
      Not_found ->
	if IdentSet.mem i fv_to_change
	then self#prim  ( Pfield ( self#v2f i)) [super#var switch_ident]
	else super#var i

  method! func kind args body =
    (* should make the function unary *)
    let arg =
      match args with
      | [a] -> a
      | _ -> assert false in
    let f = super#func kind args body in
    let fv = free_variables f in
    (* if IdentSet.is_empty fv *)
    (* then *)
    (*   begin *)
    (* 	(\* let save_arg = argument_var in *\) *)
    (* 	(\* argument_var <- arg; *\) *)
    (* 	let body' = self#lambda body in *)
    (* 	(\* argument_var <- save_arg; *\) *)
    (* 	let c = bigswitch.sw_numconsts in *)
    (* 	let body'' = *)
    (* 	  Llet ( Alias, arg, (super#var arg_ident), body') in *)
    (* 	bigswitch <- *)
    (* 	  { bigswitch with *)
    (* 	    sw_numconsts = succ c; *)
    (* 	    sw_consts = (c, body'') :: bigswitch.sw_consts; *)
    (* 	  }; *)
    (* 	Lconst ( Const_base (Asttypes.Const_int c)) *)
    (*   end *)
    (* else *)
      begin
	let fvl = IdentSet.elements fv in
	let save_fv_to_change = fv_to_change
	and save_fv_idents = fv_idents in
	fv_to_change <- fv;
	fv_idents <- fvl;
	(* let save_arg = argument_var in *)
	(* argument_var <- arg; *)
	let body' = match f with Lfunction ( _, _, b) -> b | _ -> assert false in
	fv_to_change <- save_fv_to_change;
	fv_idents <- save_fv_idents;
	(* argument_var <- save_arg; *)
	let body'' =
	  Llet ( Alias, arg, (super#var arg_ident), body') in
	let c = bigswitch.sw_numconsts in
	bigswitch <-
	  { bigswitch with
	    sw_numconsts = succ c;
	    sw_consts = ( c, body'') :: bigswitch.sw_consts;
	  };
	Lprim (
	  Pmakeblock ( 0, Asttypes.Immutable),
	  (
	    (Lconst ( Const_base (Asttypes.Const_int c)))
	    :: List.map super#var fvl
	  )
	)
      end

  method! apply f args loc =
    let f = self#lambda f in
    let arg =
      match args with
      | [a] -> self#lambda a
      | _ -> assert false in
    Lapply ( Lvar apply_ident, [f;arg], loc)

  (* Maybe I should handle the ugly rec construction *)
  (* method! letrec l body = *)
    

  method! letin k i lam lin =
    match lam with
      Lfunction (fk,fis,fl) ->
	let block = self#func fk fis fl in
	funs <- Imap.add i block funs;
	Llet ( k, i, block, self#lambda lin)
    | _ -> super#letin k i lam lin


(* The method to call at the end of the map *)
  method mk_apply l =
    let switch =
      {bigswitch with
	sw_consts = List.rev bigswitch.sw_consts;
	sw_blocks = List.rev bigswitch.sw_blocks;
      } in
    Lletrec (
      [ apply_ident,
	Lfunction (
	  Curried, [ switch_ident; arg_ident],
	  Lswitch ( Lprim ( (Pfield 0), [Lvar switch_ident]), switch)
	)
      ],
      l
    )
      
end

class unarizer i =
object (self)

  val mutable globals = []

  val mutable last_i = i
  method mk_ident =
    last_i <- succ last_i;
    { stamp = last_i; name = "#i"; flags = 0 }

  inherit Lmapper.mapper (*transformer*) as super

  method! func k args body =
    match args with
      [] -> assert false
    | a :: [] -> super#func k [a] body
    | hd :: tl -> super#func k [hd] (self#func k tl body)

  method! apply f args loc =
    let rec aux f args =
      match args with
      | [] -> assert false
      | _ :: [] -> super#apply f args loc
      | hd :: tl -> aux (super#apply f [hd] loc) tl in
    aux f args

  method! send kind obj meth args loc =
    let i_obj = self#mk_ident in
    let i_meth = self#mk_ident in
    let i_args = List.rev_map (fun _ -> self#mk_ident) args in
    self#apply (self#func Curried (i_obj::i_meth::i_args) (super#send kind (self#var i_obj) (self#var i_meth) (List.map self#var i_args) loc)) (obj::meth::args) loc

  method! prim p l =
    match p,l with
      Psetglobal i, _ -> globals <- (i,l) :: globals; super#prim p l
    | Pfield n, [Lprim (Pgetglobal i,[])] ->
      begin
      try ( List.nth ( List.assoc i globals) n ) with
      | Not_found -> super#prim p l
      end
    | _ -> super#prim p l

end

let unglobalize lambda i =
  let u = new unarizer i in
  let lambda' = u#lambda lambda in
  let o = new transformer in
  o#mk_apply ( o#lambda lambda') (* for safety, we should do 2 maps *)

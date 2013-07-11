open Lambda
open Ident

let noarg = { stamp = 0; name = ""; flags = 0 }
let switch_ident = { stamp = max_int; name = "#switch_f"; flags = 0}
let apply_ident = { stamp = pred max_int; name = "#apply_f"; flags = 0}
let arg_ident = { stamp = pred ( pred max_int); name = "#arg_f"; flags = 0}

class transformer =
object (self)
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
    v2f_aux v 0 fv_idents

(*  method mk_construct l =
    counter <- succ counter;
    Lprim ( Pmakeblock (counter, Asttypes.Immutable), l) *)


  inherit Lmapper.mapper as super


  method! var i =
    if IdentSet.mem i fv_to_change
    then self#prim  ( Pfield ( self#v2f i)) [super#var switch_ident]
    else
      if i = argument_var
      then super#var arg_ident
      else super#var i

  method! func kind args body =
    (* should make the function unary *)
    let arg = match args with [a] -> a | _ -> assert false in
    let f = Lfunction ( kind, args, body) in
    let fv = free_variables ( f) in
    if IdentSet.is_empty fv
    then
      begin
	let c = bigswitch.sw_numconsts in
	let save_arg = argument_var in
	argument_var <- arg;
	bigswitch <-
	  { bigswitch with
	    sw_numconsts = succ c;
	    sw_consts = (c,self#lambda body) :: bigswitch.sw_consts;
	  };
	argument_var <- save_arg;
	Lconst ( Const_base (Asttypes.Const_int c))
      end
    else
      begin
	let c = bigswitch.sw_numblocks in
	let fvl = IdentSet.elements fv in
	let save_fv_to_change = fv_to_change
	and save_fv_idents = fv_idents in
	fv_to_change <- fv;
	fv_idents <- fvl;
	let save_arg = argument_var in
	argument_var <- arg;
	let body = self#lambda body in
	fv_to_change <- save_fv_to_change;
	fv_idents <- save_fv_idents;
	argument_var <- save_arg;
	bigswitch <-
	  { bigswitch with
	    sw_numblocks = succ c;
	    sw_blocks = (c, body) :: bigswitch.sw_blocks;
	  };
	Lprim ( Pmakeblock (c, Asttypes.Immutable), List.map super#var fvl)
      end

  method! apply f args loc =
    let f = self#lambda f in
    let args = List.map self#lambda args in
    Lapply ( Lvar apply_ident, f::args, loc)
  (* not good, should be unary function ! *)


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
	  Lswitch ( Lvar ( switch_ident), switch)
	)
      ],
      l
    )
      
end

class unarizer i =
object (self)

  val mutable last_i = i
  method mk_ident =
    last_i <- succ last_i;
    { stamp = last_i; name = "#i"; flags = 0 }

  inherit transformer as super

  method! func k args body =
    match args with
      [] -> assert false
    | _ :: [] -> super#func k args body
    | hd :: tl -> Lfunction ( k, [hd], self#func k tl body)

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

end

let unglobalize lambda i =
  let o = new unarizer i in
  o#mk_apply ( o#lambda lambda) (* for safety, we should do 2 maps *)

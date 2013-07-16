open Lambda
open Ident

module Imap = Map.Make ( struct type t = Ident.t let compare a b = compare a.stamp b.stamp end )

let noarg = { stamp = 0; name = ""; flags = 0 }
let switch_ident = { stamp = max_int; name = "#switch_f"; flags = 0}
let apply_ident = { stamp = pred max_int; name = "#apply_f"; flags = 0}
let arg_ident = { stamp = pred ( pred max_int); name = "#arg_f"; flags = 0}

class transformer =
object (self)

  val mutable in_function = false

  val mutable funs = (Imap.empty : lambda Imap.t)

  val mutable bigswitch =
    {
      sw_numconsts = 0;
      sw_consts = [];
      sw_numblocks = 0;
      sw_blocks = [];
      sw_failaction = None;
    }

  val mutable fv = IdentSet.empty
  val mutable fv_idents = []
  val mutable fv_num = 0
  val mutable nonfree_vars = IdentSet.empty
  val mutable recvars = []

  method boundvar i = nonfree_vars <- IdentSet.add i nonfree_vars

  method private v2f v =
    let rec v2f_aux v r = function
      | [] -> raise Not_found
      | h :: t when h = v -> r
      | _ :: t -> v2f_aux v (pred r) t
    in
    v2f_aux v fv_num fv_idents
  (* field 0 is the func ident *)


  inherit Lmapper.mapper as super


  method! var i =
    if in_function
    then
      try Imap.find i funs with
	Not_found ->
	  if IdentSet.mem i nonfree_vars 
	  then Lvar i
	  else
	    if IdentSet.mem i fv
	    then Lprim  ( Pfield ( self#v2f i), [Lvar switch_ident])
	    else
	      begin
		fv <- IdentSet.add i fv;
		fv_idents <- i :: fv_idents;
		fv_num <- succ fv_num;
		Lprim (Pfield fv_num, [super#var switch_ident])
	      end
    else Lvar i

  method! func kind args body =
    let fv_save = fv in
    fv <- IdentSet.empty;
    let fv_idents_save = fv_idents in
    fv_idents <- [];
    let fv_num_save = fv_num in
    fv_num <- 0;
    let nonfree_vars_save = nonfree_vars in
    let arg = List.hd args in
    nonfree_vars <- IdentSet.singleton arg;
    let in_function_save = in_function in
    in_function <- true;
    let body' = self#lambda body in
    fv <- fv_save;
    let fvl = List.rev fv_idents in
    fv_idents <- fv_idents_save;
    fv_num <- fv_num_save;
    nonfree_vars <- nonfree_vars_save;
    in_function <- in_function_save;
    let body'' = Llet ( Alias, arg, (Lvar arg_ident), body') in
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

  method! apply f args loc =
    let f = self#lambda f in
    let arg =
      match args with
      | [a] -> self#lambda a
      | _ -> assert false in
    Lapply ( Lvar apply_ident, [f;arg], loc)

  (* Maybe I should handle better the rec construction *)
  method! letrec l body =
    let recvars_save = recvars in
    let rec map_append l res =
      match l with
      | (a,_)::tl -> map_append tl ( a :: res)
      | [] -> res
    in
    recvars <- map_append l recvars;
    let l = List.map (fun (i,lam) -> (i,self#lambda lam)) l in
    recvars <- recvars_save;
    List.iter (fun (i,_) -> self#boundvar i) l;
    Lletrec ( l, self#lambda body)
    

  method! letin k i lam lin =
    match lam with
      Lfunction (fk,fis,fl) ->
	let block = self#func fk fis fl in
	funs <- Imap.add i block funs;
	Llet ( k, i, block, self#lambda lin)
    | _ ->
      self#boundvar i;
      super#letin k i lam lin


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

  inherit Identer.reidenter i

  val mutable globals = ([] : ( Ident.t * lambda ) list )

  inherit Lmapper.mapper as super

  method! func k args body =
    match args with
      [] -> assert false
    | a :: [] ->
      let a = self#ident a in
      Lfunction ( k, [a], self#lambda body)
    | hd :: tl ->
      let a = self#ident hd in
      super#func k [a] (self#func k tl body) (* is that smart ? *)

  method! apply f args loc =
    let rec aux f args =
      match args with
      | [] -> assert false
      | _ :: [] -> super#apply f args loc
      | hd :: tl -> aux (super#apply f [hd] loc) tl in
    aux f args

  method! send kind obj meth args loc =
    let i_obj = self#mk_ident "#object" in
    let i_meth = self#mk_ident "#method" in
    let i_args = List.rev_map (fun _ -> self#mk_ident "#argument") args in
    self#apply
      (self#func
	 Curried
	 (i_obj::i_meth::i_args)
	 (super#send kind (Lvar i_obj) (Lvar i_meth) (List.map (fun i -> Lvar i) i_args) loc)
      )
      (obj::meth::args)
      loc

  method! prim p l =
    match p,l with
    | Pgetglobal i, []  -> self#var i
    | _ -> super#prim p l

  method! var i = Lvar (self#ident i)
  method! letin k i e b = super#letin k (self#ident i) e b
  method! letrec l b = super#letrec (List.map (fun ( i, e) -> (self#ident i, e)) l) b
  method! trywith l i l2 = super#trywith l (self#ident i) l2
  method! fordo id = super#fordo (self#ident id)
  method! assign i = super#assign (self#ident i)
  method! ifused id = super#ifused (self#ident id)

  method register_global i ll =
    globals <- (i,ll) :: globals

end

let unglobalize lambdas =
  let u = new unarizer 1000 in
  let rec aux i =
    if i = pred (Array.length lambdas)
    then
      match u#lambda lambdas.(i) with
      | Lprim (Psetglobal id, ( [lam]) ) ->
	u#register_global id lam;
	lam
      | _ -> assert false
    else
      let l = u#lambda lambdas.(i) in
      u#clear;
      match l with
      | Lprim (Psetglobal id, [lam]) ->
	u#register_global id lam;
	Llet ( Alias, id, lam, aux (succ i))
      | _ -> assert false
  in
  let lambda' = aux 0 in
  let o = new transformer in
  o#mk_apply ( o#lambda lambda')

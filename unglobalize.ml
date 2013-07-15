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

  val mutable fv = IdentSet.empty
  (* val mutable fv_to_change = IdentSet.empty *)
  val mutable fv_idents = []
  val mutable fv_num = 0
  val mutable nonfree_vars = IdentSet.empty
  (* val mutable argument_var = noarg *)

  method boundvar i = nonfree_vars <- IdentSet.add i nonfree_vars

  method private v2f v =
    let rec v2f_aux v r = function
      | [] -> raise Not_found
      | h :: t when h = v -> r
      | _ :: t -> v2f_aux v (pred r) t
    in
    v2f_aux v fv_num fv_idents (* field 0 is the func ident *)


  inherit Lmapper.mapper as super


  method! var i =
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
    let body' = self#lambda body in
    fv <- fv_save;
    let fvl = List.rev fv_idents in
    fv_idents <- fv_idents_save;
    fv_num <- fv_num_save;
    nonfree_vars <- nonfree_vars_save;
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

  (* Maybe I should handle the ugly rec construction *)
  method! letrec l body =
    List.iter (fun (i,_) -> self#boundvar i) l;
    super#letrec l body
    

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

  val mutable globals = []

  inherit Lmapper.mapper as super

  method! func k args body =
    match args with
      [] -> assert false
    | a :: [] ->
      let a = self#ident a in
      Lfunc ( k, [a], self#lambda body)
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
      Psetglobal i, _ ->
	(* Printf.printf "New global %s/%d\n" i.name i.stamp; *)
	let i = self#ident i in
	globals <- (i,l) :: globals; super#prim p l
    | Pfield n, [Lprim (Pgetglobal i,[])] ->
      begin
	let i = self#ident i in
	try ( List.nth ( List.assoc i globals) n ) with
	| Not_found -> (* print_endline "fail !"; *) super#prim p l
      end
    | _ -> super#prim p l

  method! var i = Lvar (self#ident i)
  method! letin k i e b = super#letin k (self#ident i) e b
  method! letrec l b = super#letrec (List.map (fun ( i, e) -> (self#ident i, e)) l) b
  method! trywith l i l2 = super#trywith l (self#ident i) l2
  method! fordo id = super#fordo (self#ident id)
  method! assign i = super#assign (self#ident i)
  method! ifused id = super#ifused (self#ident id)

end

let unglobalize lambda i =
  let u = new unarizer i in
  (* print_endline "Unarized !"; *)
  let lambda' = u#lambda lambda in
  let o = new transformer in
  o#mk_apply ( o#lambda lambda') (* for safety, we should do 2 maps *)

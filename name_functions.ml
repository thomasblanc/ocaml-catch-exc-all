(* Not sure it'll be usefull but I keep it here just in case: it puts a name on every function the typer identified *)


open Typedtree

let map2 f g l = List.map (fun (a,b) -> (f a, g b)) l


class mapping i =
object (self)
  inherit Identer.mapper i as super
  method ! expression = function
  (* if it's a let binding, we won't rename the functions under it *)
  | { exp_desc = Texp_let (flag,l,e); _ } as cont ->
    { cont with
      exp_desc = Texp_let 
	( flag,
	  List.map
	    (function p,( {exp_desc = Texp_function (label, list, partial) } as e2)->
	      (self#pattern p,
	       { e2 with
		 exp_desc = Texp_function ( label, map2 self#pattern self#expression list, partial);
		 exp_extra = List.map (fun (a,b) -> (self#exp_extra a, b)) e2.exp_extra;
		 exp_env = self#env e2.exp_env;
	       })
	    | p,e -> (self#pattern p, self#expression e)
	    )
	    l,
	  e);
      exp_extra = List.map (fun (a,b) -> (self#exp_extra a, b)) cont.exp_extra;
    }
  (* if it's an unnamed function, we need to ident it.
     (name is f, a good random function must always be named f) *)
  | { exp_desc = Texp_function (label, list, partial);
      exp_env = env;
      exp_type = typ;
      _ } as cont ->
    let i = self#mk_ident "f" in
    let exp_extra = List.map (fun (a,b) -> (self#exp_extra a, b)) cont.exp_extra in
    { cont with
      exp_extra;
      exp_desc =
	Texp_let 
	  (Asttypes.Nonrecursive,
	   [ Helpers.mk_pat ~typ ~env ( Tpat_var( i, Location.mknoloc "")),
	     { cont with
	       exp_extra;
	       exp_desc = Texp_function (label, map2 self#pattern self#expression list, partial)
	     }
	   ],
	   Helpers.mk_expr ~typ ~env
	     ( Texp_ident
		 (Path.Pident i,
		  mknoloc (Longident.Lident "f"),
		  Types.({val_type = typ; val_kind = Val_reg; val_loc = Location.none }) ))
	  );
    }
  (* Well, we go down in the tree *)
  | e -> super#expression e
  
end

let name_functions t i =
  let m = (new mapping i) in
  let t' = m # structure t in
  (t', m # last_id)
    

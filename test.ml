(* Another test, I like tests. *)




open Tt_mapper
open Cmt_format
open Types
let map_ref f r = ref (f !r)
let map_option f = function Some x -> Some (f x) | None -> None
let map_meths f = Meths.mapi (fun _ x -> f x)
let map f l = List.rev (List.rev_map f l)
let map_tuple f g (a,b) = (f a, g b)
let map2 f g = map (map_tuple f g)
let map3 f g h = map (fun (a,b,c)->(f a, g b, h c))
let map4 f g h i = map (fun (a,b,c,d)->(f a, g b, h c,i d))
let map5 f g h i j = map (fun (a,b,c,d,e)->(f a, g b, h c,i d,j e))
let id x = x
let print_endline s = print_endline s; flush stdout

class printer =
object (self)
  inherit mapper as super
  val table = Hashtbl.create 50
  (*method! pattern pat =
    print_endline "pattern"; super#pattern pat
  method! expression e =
    print_endline "expression"; super#expression e
  method! class_expr cl =
    print_endline "class"; super#class_expr cl
  method! structure s =
    Printf.printf "structure: %d\n%!" (List.length s.Typedtree.str_items);
    super#structure s
  method! structure_item s =
    print_endline "item"; super#structure_item s
  method! t_signature s =
    Printf.printf "tsig: %d\n%!" (List.length s); super#t_signature s
  method! env e =
    print_endline "env"; super#env e
  method! t_signature_item i =
    print_endline "sitem"; super#t_signature_item i
  method! t_type_declaration td =
    print_endline "tdecl"; super#t_type_declaration td
  method! t_class_declaration cd =
    print_endline "cdecl"; super#t_class_declaration cd *)
  method! t_type_expr e =
    (*Printf.printf "id: %d, lvl: %d\n" e.id e.level;*)
    if Hashtbl.mem table e.id
    then print_endline "MEURS !!!!"
    else Hashtbl.add table e.id ();
    let e = super#t_type_expr e in
    Hashtbl.remove table e.id;
    e
    
(*  method! t_type_desc = function
  | Tarrow ( label, e1, e2, c ) -> print_endline "arrow"; Tarrow ( label , self#t_type_expr e1 , self#t_type_expr e2 , c)
  | Ttuple ( l ) -> print_endline "tuple"; Ttuple ( map self#t_type_expr l )
  | Tconstr ( p, l, memo ) -> print_endline "constr"; Tconstr ( self#path p , map self#t_type_expr l, map_ref self#t_abbrev_memo memo )
  | Tobject ( e, o ) -> print_endline "object"; Tobject ( self#t_type_expr e , map_ref (map_option (map_tuple self#path (map self#t_type_expr))) o )
  | Tfield ( s, k, e1, e2 ) -> print_endline "field"; Tfield ( s , k , self#t_type_expr e1 , self#t_type_expr e2 )
  | Tlink ( e ) -> print_endline"link"; Tlink ( self#t_type_expr e )
  | Tsubst ( e ) -> print_endline"subst"; Tsubst ( self#t_type_expr e )
  | Tvariant ( r ) -> print_endline"variant"; Tvariant ( self#t_row_desc r )
  | Tpoly ( e,l ) -> print_endline"poly"; Tpoly ( self#t_type_expr e, map self#t_type_expr l )
  | Tpackage ( p, l, exprs ) -> print_endline"pack"; Tpackage ( self#path p , map self#longident l , map self#t_type_expr exprs )
  | d -> print_endline"other"; d*)

  (*method! t_type_desc d =
    print_endline "t_type_desc"; super#t_type_desc d*)
  (*
  method! path p =
    (match p with
      Path.Pdot (_,s,i) -> Printf.printf "Path: %s, %d\n" s i
    | _ -> ());
    super#path p

  method! ident i =
    Printf.printf "Ident: %d, %s\n" i.Ident.stamp i.Ident.name;
    super#ident i
  *)
  (*method! t_class_type ct =
    print_endline "t_c_t"; super#t_class_type ct *)
end

let p = new printer

let _ = match (read_cmt Sys.argv.(1)).cmt_annots with
    Implementation s -> p#structure s
  | _ -> assert false


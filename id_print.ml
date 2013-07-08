(* This code is for use on VERY small examples (eg. itself, though it's big) *)
(* It does print all the Idents.t it encouters in a cmt given on argv.(1). And there are a lot of those. *)
open Tt_mapper
open Cmt_format
open Types

(* For testing purpose *)
module T = struct
  type t = A|B
  let f = function A -> true | B -> false
end

let g = T.f

class test = object method t = 0 end
(* Can someone tell me why an object creates three identifiers ?
   I see :
   - the constructor
   - the type
   - the #type
   What's the fourth for ? *)

(* The actual code *)

class reident =
object (self)
  inherit mapper as dad

  method! ident i = Ident.(Printf.printf "stamp: %d; name: %s; flags: %x\n" i.stamp i.name i.flags); dad#ident i
  method! path = let open Path in function
  | Pident i -> Pident (self#ident i)
  | Pdot (p,s,i) -> Printf.printf "Path to %s, %d\n" s i; Pdot (self#path p, s, i)
  | Papply (p1,p2) -> Papply (self#path p1, self#path p2)

end

let r = new reident

let _ = match (read_cmt Sys.argv.(1)).cmt_annots with
  | Implementation s -> r#structure s
  | _ -> assert false

(* This is a test file *)

class t =
object (self)
  method a : String.t = ""
  method b = self#c
  method c = self#d
  method d = self#e
  method e = self#f
  method f = self#a
end 
(*
type t = A(* of t*)
*)
(*
type t1 = A of t2
and t2 = B of t1
class t = object end

type z = [ `A | `B]
*)

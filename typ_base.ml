(*
  There was a time I thought I should store the types somewhere, so that I can reuse the information.
  Then I stopped in the middle of the code to do something else
  Then I decided "who cares about types anyway ?"
  Then I let it here, just in case I do need to get types anyway.
*)
module Typs = Map.Make (
  struct
    type t = Ident.t
    let compare a b = compare a.Ident.stamp b.Ident.stamp
  end )

let make_typs ttree =
  let open Typedtree in
  let mapper = 
object
  inherit Tt_mapper.mapper as super
  val mutable types =
    Typs.empty : Types.type_expr Typs.t (* builtins ! *)
  val mutable mtypes =
    Typs.empty : Types.module_type Typs.t (* builtins ! *)
  method typs = types
  method mtyps = mtypes

  method ! pattern p =
    match p with
    | { } ->
    | _ -> super#pattern p

end
  in
  mapper#structure ttree;
  ( mapper#typs , mapper#mtyps)
  
let is_func typs i =
  try
    let typ = Typs.find i typs in
    match (*Ctype.repr(* done previously *)*) typ with
      Types.Tarrow _ -> true
    | _ -> false
  with Not_found -> false

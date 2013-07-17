(* Step one: load  the cmts, turn them to lambda *)
(* Now we just load them, merging later *)

let () = print_endline "Step 1"

(* a little clean could make it simpler *)
let lambdas =
  Arguments.iterate
    (fun ( name, tree) ->
      Translmod.transl_implementation name
	( tree, Typedtree.Tcoerce_none)
    )

(* Step two: merge the lambda code
   - removing the "global" primitive
   - putting all functions on top of everything
*)

let () = print_endline "Step 2"

let lambda = Unglobalize.unglobalize lambdas

(* for debugging purpose, this shall not stay here in final code *)
let () =
  if Arguments.print ()
  then Printlambda.lambda Format.std_formatter lambda;
  if Arguments.comp ()
  then Compilelambda.comp lambda;

(* Step three: analysis *)
(* (* Step four: compile ? *) *)

(* Step one: load  the cmts, turn them to lambda *)
(* Now we just load them, merging later *)

(* a little clean could make it simpler *)
let lambdas =
  Arguments.iterate
    (fun ( name, tree) ->
      Translmod.transl_implementation name
	( tree, Typedtree.Tcoerce_none)
    )

let () =
  if lambdas = [| |]
  then print_endline "You must specify at least one cmt file"
  else
    begin

      (* Step two: merge the lambda code
	 - removing the "global" primitive
	 - putting all functions on top of everything
      *)

      let lambda = Unglobalize.unglobalize lambdas in

      (* for debugging purpose, this shall not stay here in final code *)
      if Arguments.print ()
      then Printlambda.lambda Format.std_formatter lambda;
      if Arguments.comp ()
      then Compilelambda.comp lambda;

    (* Step three: analysis *)
    (* (* Step four: compile ? *) *)
    end

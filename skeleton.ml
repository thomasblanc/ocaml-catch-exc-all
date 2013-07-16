(* I should, someday, put the main code in here, here's the trailer: *)

(* Step one: merge the cmts *)
(* Now we just load them, merging later *)

let () = print_endline "Step 1"

let load_tt s =
  let open Cmt_format in
  let cmt = read_cmt s in
  match cmt.cmt_annots with
    Implementation str -> Tt_restore.restore s str cmt.cmt_modname
  | _ -> assert false

(*let typedtrees =
  let open Sys in
  let open Array in
  (* Idents.merge_cmts ( sub argv 1 ( pred ( length argv))) *)
  map load_tt ( sub argv 1 ( pred ( length argv)))

(* let () = Printtyped.implementation Format.std_formatter typedtree *)

(* Step two: put a name on every functions *)

(* let ( typedtree, i) = Name_functions.name_functions typedtree i *)

(* Step three: go to lambda code *)

let () = print_endline "Step 3"
*)
let lambdas =
  Arguments.iterate
  (* Array.map *)
    (fun ( name, tree) ->
      Translmod.transl_implementation name ( tree, Typedtree.Tcoerce_none)
    ) (* typedtrees *)

(* Step four: globalize the functions, unglobalize the non-func-values *)
(* Maybe I should remove that structure around first ? Yes ? No ? Maybe ? *)
(* This step has been composed with : *)
(* Step five: start the big rec switch *)

let () = print_endline "Step 4 and 5"

let lambda = Unglobalize.unglobalize lambdas

(* for debugging purpose *)
let () = Printlambda.lambda Format.std_formatter lambda

(* Step six: analysis *)
(* (* Step seven: compile ? *) *)

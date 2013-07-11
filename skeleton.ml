(* I should, someday, put the main code in here, here's the trailer: *)

(* Step one: merge the cmts *)

let () = print_endline "Step 1"

let ( typedtree, i) =
  let open Sys in
  let open Array in
  Idents.merge_cmts ( sub argv 1 ( pred ( length argv)))

(* Step two: put a name on every functions *)

(* let ( typedtree, i) = Name_functions.name_functions typedtree i *)

(* Step three: go to lambda code *)

let () = print_endline "Step 3"

let lambda = Translmod.transl_implementation "Mod" ( typedtree, Typedtree.Tcoerce_none)

(* Step four: globalize the functions, unglobalize the non-func-values *)
(* Maybe I should remove that structure around first ? Yes ? No ? Maybe ? *)
(* This step has been composed with : *)
(* Step five: start the big rec switch *)

let () = print_endline "Step 4 and 5"

let lambda = Unglobalize.unglobalize lambda i

(* for debugging purpose *)
let () = Printlambda.lambda Format.std_formatter lambda

(* Step six: analysis *)
(* (* Step seven: compile ? *) *)

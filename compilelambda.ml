open Misc
open Config
open Format
open Typedtree
open Compenv

let print_if ppf flag printer arg =
  if !flag then fprintf ppf "%a@." printer arg;
  arg

let (++) x f = f x
let (+++) (x, y) f = (x, f y)

let outputprefix = "lambda_out"

let ppf = Format.std_formatter

let comp lambda =
  
(* let implementation ppf sourcefile outputprefix = *)
(*   Location.input_name := sourcefile; *)
(*   Compmisc.init_path true; *)
(*   let modulename = *)
(*     String.capitalize(Filename.basename(chop_extension_if_any sourcefile)) in *)
(*   check_unit_name ppf sourcefile modulename; *)
(*   Env.set_unit_name modulename; *)
(*   let inputfile = Pparse.preprocess sourcefile in *)
(*   let env = Compmisc.initial_env() in *)
(*   Compilenv.reset ?packname:!Clflags.for_package modulename; *)
  let cmxfile = outputprefix ^ ".cmx" in
  (* let objfile = outputprefix ^ ext_obj in *)
(*   try *)
(*     if !Clflags.print_types then ignore begin *)
(*       Pparse.file ppf inputfile Parse.implementation ast_impl_magic_number *)
(*       ++ print_if ppf Clflags.dump_parsetree Printast.implementation *)
(*       ++ print_if ppf Clflags.dump_source Pprintast.structure *)
(*       ++ Typemod.type_implementation sourcefile outputprefix modulename env *)
(*       ++ print_if ppf Clflags.dump_typedtree *)
(*                   Printtyped.implementation_with_coercion *)
(*     end else begin *)
(*       Pparse.file ppf inputfile Parse.implementation ast_impl_magic_number *)
(*       ++ print_if ppf Clflags.dump_parsetree Printast.implementation *)
(*       ++ print_if ppf Clflags.dump_source Pprintast.structure *)
(*       ++ Typemod.type_implementation sourcefile outputprefix modulename env *)
(*       ++ print_if ppf Clflags.dump_typedtree *)
(*                   Printtyped.implementation_with_coercion *)
(*       ++ Translmod.transl_store_implementation modulename *)
(*       +++ print_if ppf Clflags.dump_rawlambda Printlambda.lambda *)
	(0,lambda)
      +++ Simplif.simplify_lambda
      (* +++ print_if ppf Clflags.dump_lambda Printlambda.lambda *)
      ++ Asmgen.compile_implementation outputprefix ppf;
      Compilenv.save_unit_info cmxfile;
  (*   end; *)
  (*   Warnings.check_fatal (); *)
  (*   Pparse.remove_preprocessed inputfile; *)
  (*   Stypes.dump (Some (outputprefix ^ ".annot")); *)
  (* with x -> *)
  (*   remove_file objfile; *)
  (*   remove_file cmxfile; *)
  (*   Pparse.remove_preprocessed inputfile; *)
  (*   Stypes.dump (Some (outputprefix ^ ".annot")); *)
  (*   raise x *)

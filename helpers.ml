open Typedtree

let noneloc = Location.none

let mk_pat ?(loc = noneloc) ?(extra = []) ~typ ~env desc =
  { pat_desc = desc;
    pat_type = typ;
    pat_extra = extra;
    pat_loc = loc;
    pat_env = env; }

let mk_expr ?(loc = noneloc) ?(extra = []) ~typ ~env desc =
  { exp_desc = desc;
    exp_type = typ;
    exp_extra = extra;
    exp_loc = loc;
    exp_env = env; }

open Predef
open Env
let core_env = initial


let typ_unit = type_unit
let typ_int =  type_int
let val_unit = find_value ( Path.Pident ( List.assoc "()" builtin_idents) ) core_env
let cstr_unit =
  let open Types in
  {
    cstr_name = "()";
    cstr_res = typ_unit;                (* Type of the result *)
    cstr_existentials = [];  (* list of existentials *)
    cstr_args = [];          (* Type of the arguments *)
    cstr_arity = 0;                    (* Number of arguments *)
    cstr_tag = Cstr_constant 0;          (* Tag for heap blocks *)
    cstr_consts = 1;                   (* Number of constant constructors *)
    cstr_nonconsts = 0;                (* Number of non-const constructors *)
    cstr_normal = 0;                   (* Number of non generalized constrs *)
    cstr_generalized = true;             (* Constrained return type? *)
    cstr_private = Asttypes.Public }        (* Read-only constructor? *)
    


let typ_arrow ?(label="") ?(commutable=Types.Cunknown) t1 t2 =
  let open Types in
  {
    desc = Tarrow ( label, t1, t2, commutable);
    level = 0; (* UNSAFE ! *)
    id = 0;
  }

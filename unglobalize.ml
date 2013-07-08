

let rec global_funcs typs e = (* take every functions and put them in top of everything *)
  match e with
  | _ -> e

let rec remove_free_vars e = (* see lambda.mli *)
  match e with
  | Lapply of ( func, args, loc) -> raise Not_implemented
  | Lfunction ( kind, args, lambda) -> raise Not_implemented
  | _ -> e

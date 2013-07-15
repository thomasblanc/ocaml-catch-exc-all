open Ident

class identer id =
object
  val mutable last_id = id
  method mk_ident ?(flags = 0) s =
    last_id <- succ last_id;
    { stamp = last_id; name = s; flags; }
  method last_id = last_id
end

module Imap = Map.Make ( struct t = int let compare = compare end)

class reidenter id =
object
  inherit identer id
  val mutable idented = Imap.empty
  method clear = idented <- Imap.empty
  method ident i =
    if i.stamp < 1000
    then i
    else
      try Imap.find i.stamp idented with
	Not_found ->
	  let id = mk_ident ~flags=i.flags i.name in
	  idented <- Imap.add i.stamp id;
	  id
end

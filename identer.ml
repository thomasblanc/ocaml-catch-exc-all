class mapper id =
object
  val mutable last_id = id
  inherit Tt_mapper.mapper
  method mk_ident ?(flags = 0) s =
    last_id <- succ last_id;
    Ident.({ stamp = last_id; name = s; flags; })
  method last_id = last_id
end

    

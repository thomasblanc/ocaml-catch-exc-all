open Tt_mapper

class restorer =
object
  inherit mapper
  method! env e = Envaux.env_of_only_summary e
end

let restore = 
  let r = new restorer in
  r # structure
  

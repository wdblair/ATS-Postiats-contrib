
implement
parse_valkind(js) = let
  val knd = parse_string(js)
in
  case+ knd of
    | "VK_val" => VK_val()
    | "VK_val_pos" => VK_val_pos()
    | "VK_val_neg" => VK_val_neg()
    | "VK_prval" => VK_prval()
    | _ => $raise InvalidJSON()
end

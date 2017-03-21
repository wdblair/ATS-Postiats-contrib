
#define :: list_cons

fun
parse_P2Tvar(js: jsonval): p2at_node = let
  val- JSONarray(jss) = js
  val- JSONobject(attrs) :: _ = jss
  val- (_, JSONint(id)) :: _ = attrs
  val id' = $UN.cast{int}(id)
  val stamp = stamp_make(id')
  val d2v = d2var_of_stamp_exn(stamp)
in
  P2Tvar(d2v)
end

implement
parse_p2at(js) = let
  val- JSONobject(attrs) = js
  val- (_, loc) :: (_, node) :: _ = attrs
  val- JSONobject(nodeattrs) = node
  val- (tag, con) :: _ = nodeattrs
  val p2at_node =
    (case+ tag of
      | "P2Tvar" => parse_P2Tvar(con)
      | _ => P2Terrpat()): p2at_node
in
  '{ p2at_loc = parse_location(loc),
     p2at_svs= list_nil(),
     p2at_dvs= list_nil(),
     p2at_type = None(),
     p2at_node = p2at_node
  }
end

implement
parse_p2atlst(js) = let
  val- JSONarray(p2ats) = js
in
  list_of_list_vt(
    list_map_fun<jsonval><p2at>(p2ats, lam d => parse_p2at(d))
  )
end


#define :: list_cons

implement
parse_v2aldec(js) = let
  val- JSONobject(attrs) = js
  val- loc :: pat :: def :: ann :: _ = attrs
  val loc' = parse_location(loc.1)
  val pat' = parse_p2at(pat.1)
  val def' = parse_d2exp(def.1)
  val ann' = parse_s2expopt(ann.1)
in
 '{v2aldec_loc=loc',
   v2aldec_pat=pat',
   v2aldec_def=def',
   v2aldec_ann=ann'}
end

implement
parse_v2aldeclst(jsv) = let
  val- JSONarray(jsvs) = jsv
in
  list_of_list_vt(
    list_map_fun<jsonval><v2aldec>(jsvs, lam js => parse_v2aldec(js))
  )
end

staload "libats/ML/SATS/list0.sats"

extern
fun
parse_D2Cvaldecs(jsonval): d2ecl_node

#define :: list_cons

extern
fun
parse_D2Cnone(jsonval): d2ecl_node

extern
fun
parse_D2Clist(jsonval): d2ecl_node

extern
fun
parse_D2Cvaldecs_rec(jsonval): d2ecl_node

implement
parse_D2Cnone(jsvs) = D2Cnone()

implement
parse_D2Clist(jsv) = let
  val- JSONarray(jsvs) = jsv
  val decls = list_of_list_vt(
    list_map_fun<jsonval><d2ecl>(jsvs, lam js => parse_d2ecl(js))
  )
in
  D2Clist(decls)
end

implement
parse_D2Cvaldecs(jsv) = let
  val- JSONarray(jsvs) = jsv
  val- knd :: v2s :: _ = jsvs
  val knd'  = parse_valkind(knd)
  val v2s'  = parse_v2aldeclst(v2s)
in
  D2Cvaldecs(knd', v2s')
end

implement
parse_D2Cvaldecs_rec(jsv) = let
  val- JSONarray(jsvs) = jsv
  val- knd :: v2s :: _ = jsvs
  val knd'  = parse_valkind(knd)
  val v2s'  = parse_v2aldeclst(v2s)
in
  D2Cvaldecs_rec(knd', v2s')
end

implement
parse_d2ecl(js) = let
  val-JSONobject(d2js)          = js
  val- @(_, loc1) :: node :: _  = d2js
  val @(_, node1)               = node
  val-JSONobject(d2node)        = node1
  val- @(d2tag, d2args) :: _    = d2node
  val loc'                      = parse_location(loc1)
  val node' =
    (case+ d2tag of
      | "D2Cnone" => parse_D2Cnone(d2args)
      | "D2Clist" => parse_D2Clist(d2args)
      | "D2Cvaldecs" => parse_D2Cvaldecs(d2args)
      | "D2Cvaldecs_rec" => parse_D2Cvaldecs_rec(d2args)
      | "D2Cignored" => D2Cignored()
      | _ =>  let
        val () = prerrln!("Unknown tag:", d2tag)
      in
        $raise InvalidJSON()
      end): d2ecl_node
in
  '{
    d2ecl_loc=loc',
    d2ecl_node=node'
   }
end

implement
parse_d2eclist(json) = let

  implement
  parse_list$parse<d2ecl> = parse_d2ecl

in
  parse_list<d2ecl>(json)
end

implement
parse_filepath_d2eclist (file) = let
  val json_obj = json_object_from_file(file)
  val json = json_object2val0(json_obj)
  val () = parse_d2varmap(json)
in
  list_nil()
end

implement
parse_fileref_d2eclist(file) = list_nil()

fun
parse_D2Evar(js: jsonval): d2exp_node = let
  val- JSONarray(JSONobject(v) :: _) = js
  val- @(_, jstamp) :: _ = v
  val- JSONint(stamp) = jstamp
  val d2 = d2var_of_stamp_exn(stamp_make($UN.cast{int}(stamp)))
in
  D2Evar(d2)
end

fun
parse_D2Esing(js: jsonval): d2exp_node = let
  val- JSONarray(d2 :: _ ) = js
  val d2exp = parse_d2exp(d2)
in
  D2Esing(d2exp)
end

fun
parse_D2Elam_dyn(js: jsonval): d2exp_node = let
  val- JSONarray(JSONint(lin) :: JSONint(npf) :: p2ats :: body :: _) = js
  val lin = $UN.cast{int}(lin)
  val npf = $UN.cast{int}(npf)
  val p2ats = parse_p2atlst(p2ats)
  val body = parse_d2exp(body)
in
  D2Elam_dyn(lin, npf, p2ats, body)
end

fun
parse_D2Eapplst(args: jsonval): d2exp_node = let
  val- JSONarray(d2js :: d2argjs :: _) = args
  val d2e = parse_d2exp(d2js)
  val d2ea = parse_d2exparglst(d2argjs)
in
  D2Eapplst(d2e, d2ea)
end

fun
parse_D2EXPARGdyn(args: jsonval): d2exparg = let
  val- JSONarray(JSONint(npf) :: loc :: d2es :: _) = args
  val npf = $UN.cast{int}(npf)
  val loc = parse_location(loc)
  val d2s = parse_d2explst(d2es)
in
  D2EXPARGdyn(npf, loc, d2s)
end

fun
parse_d2exp_node(js: jsonval): d2exp_node = let
  val- JSONobject(@(tag, args) :: _) = js
in
  case+ tag of
   | "D2Evar"     => parse_D2Evar(args)
   | "D2Esing"    => parse_D2Esing(args)
   | "D2Elam_dyn" => parse_D2Elam_dyn(args)
   | "D2Eapplst"  => parse_D2Eapplst(args)
   | _ => let
     val () = prerrln!("Could not parse tag", tag)
   in
    D2Eerror()
   end
end

implement
parse_d2exp(js) = let
  val- JSONobject(@(_ , loc) :: @(_, node) :: _) = js
  val- loc'  = parse_location(loc)
  val- node' = parse_d2exp_node(node)
in
 '{
   d2exp_loc= loc',
   d2exp_node= node'
  }
end

implement
parse_d2explst(js) = let
  implement
  parse_list$parse<d2exp> = parse_d2exp

in
  parse_list<d2exp>(js)
end

implement
parse_d2exparg(js) = let
  val- JSONobject(@(tag, args) :: _) = js
in
  case+ tag of
    | "D2EXPARGdyn" => parse_D2EXPARGdyn(args)
    | _ => D2EXPARGerror()
end


implement
parse_d2exparglst(js) = let

  implement
  parse_list$parse<d2exparg> = parse_d2exparg

in
  parse_list<d2exparg>(js)
end

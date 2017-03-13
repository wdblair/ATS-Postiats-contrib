(* ****** ****** *)
//
// ATS-unjsonize-2
//
(* ****** ****** *)
//
(*
** Author: Hongwei Xi
** Authoremail: gmhwxiATgmailDOTcom
** HX-2015-08-06: start
*)
//
(* ****** ****** *)
//
#define ATS_MAINATSFLAG 1
//
#define
ATS_DYNLOADNAME
"patsunj2_parsing__dynload"
//
(* ****** ****** *)
//
#include
"share/atspre_define.hats"
#include
"share/atspre_staload.hats"
//
(* ****** ****** *)
#include
"mydepies.hats"
(* ****** ****** *)
//
staload
UN = "prelude/SATS/unsafe.sats"
//
(* ****** ****** *)
//
staload
HF = "libats/SATS/hashfun.sats"
staload
HTR = "libats/ML/SATS/hashtblref.sats"
//
(* ****** ****** *)

staload _ = "libats/ML/DATS/list0.dats"
staload _ = "libats/ML/DATS/hashtblref.dats"
staload _ = "libats/DATS/hashfun.dats"

(* ****** ****** *)

staload "{$JSONC}/SATS/json.sats"
staload "{$JSONC}/SATS/json_ML.sats"

(* ****** ****** *)

staload "./patsunj2_synent2.sats"
staload "./patsunj2_parsing.sats"

(* ****** ****** *)
//
implement
$HTR.hash_key<stamp> (x) = hash_stamp(x)
//
(* ****** ****** *)
//
implement
fprint_val<stamp> = fprint_stamp
//
implement
gequal_val_val<stamp> (x1, x2) = (x1 = x2)
//
(* ****** ****** *)

implement
fprint_val<s2cst> = fprint_s2cst
implement
fprint_val<s2var> = fprint_s2var

(* ****** ****** *)

implement
jsonval_get_field
  (jsnv, key) = let
//
(*
val () =
println!
  ("jsonval_get_field: jsnv = ", jsnv)
//
val () =
println! ("jsonval_get_field: key = ", key)
*)
//
typedef key = string
typedef itm = jsonval
//
val-JSONobject(lxs) = jsnv
//
in
  list_assoc_opt<key,itm> (lxs, key)
end // end of [jsonval_get_field]

(* ****** ****** *)
//
implement
parse_int
  (jsnv0) = let
  val-JSONint (lli) = jsnv0 in $UN.cast{int}(lli)
end // end of [parse_int]
//
implement
parse_string
  (jsnv0) = let val-JSONstring (str) = jsnv0 in str end
//
implement{t}
parse_list
  (jsnv0) = let
  val- JSONarray(jsons) = jsnv0
in
  list_of_list_vt(
    list_map_fun<jsonval><t>(jsons, lam d => parse_list$parse<t>(d))
  )
end

(* ****** ****** *)

implement
parse_stamp
  (jsnv0) = let
//
val-JSONint(lli) = jsnv0 in stamp_make($UN.cast{int}(lli))
//
end // end of [parse_stamp]

(* ****** ****** *)

implement
parse_symbol
  (jsnv0) = let
//
val-JSONstring(name) = jsnv0 in symbol_make_name (name)
//
end // end of [parse_symbol]

(* ****** ****** *)

implement
parse_location
  (jsnv0) = let
//
val-JSONstring(strloc) = jsnv0 in location_make (strloc)
//
end // end of [parse_location]

(* ****** ****** *)

implement
{a}(*tmp*)
parse_option
  (jsnv0, f) = let
//
val-JSONarray (jsnvs) = jsnv0
//
in
  case+ jsnvs of
  | list_nil () => None(*void*)
  | list_cons (jsnv, _) => Some{a}(f(jsnv))
end // end of [parse_option]

(* ****** ****** *)

implement
parse_json_file(path) = let
  val js = json_object_from_file(path)
in
  json_object2val0(js)
end

(* ****** ****** *)
//
local
//
#include
"./PARSING/patsunj2_parsing_label.dats"
//
#include
"./PARSING/patsunj2_parsing_d2var.dats"
//
#include
"./PARSING/patsunj2_parsing_d2ecl.dats"
#include
"./PARSING/patsunj2_parsing_d2exp.dats"
#include
"./PARSING/patsunj2_parsing_p2at.dats"
#include
"./PARSING/patsunj2_parsing_s2exp.dats"
#include
"./PARSING/patsunj2_parsing_valkind.dats"
#include
"./PARSING/patsunj2_parsing_v2aldec.dats" in (*nothing*)
//

end // end of [local]
//
(* ****** ****** *)

(* end of [patsunj2_parsing.dats] *)

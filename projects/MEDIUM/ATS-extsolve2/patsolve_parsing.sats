(*
** ATS-extsolve:
** For solving ATS-constraints
** with external SMT-solvers
*)

(* ****** ****** *)

(*
** Author: Hongwei Xi
** Authoremail: gmhwxiATgmailDOTcom
*)

(* ****** ****** *)
//
#include
"share/atspre_define.hats"
//
(* ****** ****** *)
//
staload
"{$JSONC}/SATS/json_ML.sats"
//
(* ****** ****** *)
//
typedef
jsnvlst = List0(jsonval)
typedef
jsnvopt = Option(jsonval)
vtypedef
jsnvopt_vt = Option_vt(jsonval)
//
(* ****** ****** *)
//
fun
jsonval_get_field
  (jsonval, string): jsnvopt_vt
//
overload [] with jsonval_get_field
//
(* ****** ****** *)

staload "./patsolve_cnstrnt.sats"

(* ****** ****** *)

fun parse_int (jsnv: jsonval): int
fun parse_string (jsnv: jsonval): string

(* ****** ****** *)

fun parse_stamp (jsnv: jsonval): stamp
fun parse_symbol (jsnv: jsonval): symbol
fun parse_location (jsnv: jsonval): loc_t

(* ****** ****** *)

fun{
a:t@ype
} parse_list
  (jsnv: jsonval, f: jsonval -> a): List0(a)
// end of [parse_list]

(* ****** ****** *)

fun{
a:t@ype
} parse_option
  (jsnv: jsonval, f: jsonval -> a): Option(a)
// end of [parse_option]

(* ****** ****** *)

fun parse_s2rt (jsnv: jsonval): s2rt
fun parse_s2rtlst (jsnv: jsonval): s2rtlst

(* ****** ****** *)
//
fun
the_s2cstmap_search(stamp): s2cstopt_vt
//
fun the_s2cstmap_insert (s2c: s2cst): void
//
(* ****** ****** *)
//
fun
the_s2varmap_search(stamp): s2varopt_vt
//
fun the_s2varmap_insert (s2v: s2var): void
//
(* ****** ****** *)
//
fun
the_s2Varmap_search(stamp): s2Varopt_vt
//
fun the_s2Varmap_insert (s2V: s2Var): void
//
(* ****** ****** *)

fun parse_s2cst (jsnv: jsonval): s2cst
fun parse_s2cstlst (jsnv: jsonval): s2cstlst

(* ****** ****** *)

fun parse_s2var (jsnv: jsonval): s2var
fun parse_s2varlst (jsnv: jsonval): s2varlst

(* ****** ****** *)

fun parse_s2Var (jsnv: jsonval): s2Var
fun parse_s2Varlst (jsnv: jsonval): s2Varlst

(* ****** ****** *)

fun parse_s2exp (jsnv: jsonval): s2exp
fun parse_s2explst (jsnv: jsonval): s2explst

(* ****** ****** *)

(* end of [patsolve_parsing.sats] *)

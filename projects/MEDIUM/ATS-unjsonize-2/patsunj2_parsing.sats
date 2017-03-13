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
#define
ATS_PACKNAME "PATSUNJ2"
//
(* ****** ****** *)
//
#include
"share/atspre_define.hats"
//
(* ****** ****** *)
#include
"mydepies.hats"
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
//
staload
"./patsunj2_synent2.sats"
//
(* ****** ****** *)

exception InvalidJSON of ()

(* ****** ****** *)

fun parse_int (jsnv: jsonval): int
fun parse_string (jsnv: jsonval): string

fun {t:t@ype}
parse_list$parse(jsonval): t

fun {t:t@ype}
parse_list (js: jsonval): List0(t)

(* ****** ****** *)

fun parse_stamp (jsnv: jsonval): stamp
fun parse_symbol (jsnv: jsonval): symbol
fun parse_location (jsnv: jsonval): loc_t

(* ****** ****** *)

fun parse_label (jsnv: jsonval): label

(* ****** ****** *)

fun{
a:t@ype
} parse_option
  (jsnv: jsonval, f: jsonval -> a): Option(a)
// end of [parse_option]

(* ****** ****** *)

fun parse_valkind(jsv: jsonval): valkind

fun parse_v2aldec(jsv: jsonval): v2aldec

fun parse_v2aldeclst(jsv: jsonval): v2aldeclst

(* ****** ****** *)

fun parse_p2at(jsv: jsonval): p2at

fun parse_p2atlst(jsv: jsonval): p2atlst

(* ****** ****** *)

fun parse_d2cst(jsv: jsonval): d2cst

fun d2cst_of_stmap_exn(stamp): d2cst

(* ****** ****** *)


fun parse_d2var (jsv: jsonval): d2var

fun parse_d2varmap(jsv: jsonval): void

fun d2var_of_stamp_exn (st: stamp): d2var

exception D2varNotDefined of (stamp)

(* ****** ****** *)

fun parse_s2rt (jsnv: jsonval): s2rt
fun parse_s2rtlst (jsnv: jsonval): s2rtlst

(* ****** ****** *)

fun parse_s2exp (jsnv: jsonval): s2exp
fun parse_s2explst (jsnv: jsonval): s2explst
fun parse_s2expopt (jsv: jsonval): s2expopt

(* ****** ****** *)

fun parse_d2exp (jsnv: jsonval): d2exp
fun parse_d2explst (jsnv: jsonval): d2explst

(* ****** ****** *)

fun parse_d2exparg (jsonval): d2exparg

(* ****** ****** *)

fun parse_d2ecl (jsnv: jsonval): d2ecl
fun parse_d2eclist (jsnv: jsonval): d2eclist

(* ****** ****** *)

fun parse_fileref_d2eclist (filr: FILEref): d2eclist

(* ****** ****** *)

fun parse_filepath_d2eclist (path: string): d2eclist

(* ****** ****** *)

fun parse_json_file(path: string): jsonval

(* end of [patsunj2_parsing.sats] *)

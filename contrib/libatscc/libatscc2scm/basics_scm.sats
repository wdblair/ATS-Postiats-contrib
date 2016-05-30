(*
** For writing ATS code
** that translates into Scheme
*)

(* ****** ****** *)
//
// HX-2015-06:
// prefix for external names
//
#define
ATS_EXTERN_PREFIX "ats2erlpre_"
//
(* ****** ****** *)
//
#include
"share/atspre_define.hats"
//
(* ****** ****** *)
//
#include "{$LIBATSCC}/basics.sats"
//
(* ****** ****** *)
//
fun
cloref0_app{b:t0p}(cfun0(b)): b = "mac#%"
//
fun
cloref1_app
  {a:t0p}{b:t0p}(cfun1(a, b), a): b = "mac#%"
//
fun
cloref2_app
  {a1,a2:t0p}{b:t0p}
  (cfun2(a1, a2, b), a1, a2): b = "mac#%"
fun
cloref3_app
  {a1,a2,a3:t0p}{b:t0p}
  (cfun3(a1, a2, a3, b), a1, a2, a3): b = "mac#%"
//
overload cloref_app with cloref0_app
overload cloref_app with cloref1_app
overload cloref_app with cloref2_app
overload cloref_app with cloref3_app
//
(* ****** ****** *)

(* end of [basics_scm.sats] *)
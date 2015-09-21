(*
##
## ATS-extsolve-smt:
## Solving ATS-constraints with arbitrary solvers
##
*)

(* ****** ****** *)
//
#ifndef
PATSOLVE_SMT_SOLVING
#include "./myheader.hats"
#endif // end of [ifndef]
//
(* ****** ****** *)
//
staload
UN = "prelude/SATS/unsafe.sats"
//
(* ****** ****** *)

abst@ype smt_context = {
    control
    output
}

extern
fun
the_smt_context_vget
(
// argumentless
) : (
  smt_context -<prf> void | smt_context
) = "ext#patsolve_the_smt_context_vget"
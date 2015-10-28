(*
##
## ATS-extsolve-smt:
## Solving ATS-constraints with SMT
##
*)

(* ****** ****** *)
//
#define
PATSOLVE_targetloc "./../.ATS-extsolve"
//
(* ****** ****** *)
//
staload
"{$PATSOLVE}/patsolve_cnstrnt.sats"
//
(* ****** ****** *)
staload
"{$PATSOLVE}/patsolve_parsing.sats"
//
staload "./../patsolve_smt_solving.sats"
//
(* ****** ****** *)

(* end of [myheader.hats] *)

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

#define :: cons_vt
#define nil nil_vt

(* ****** ****** *)

implement
smtlib_to_string
  (ast) = 
  case+ ast of
    | ~Atom (a) => a
    | ~Apply (opr, args) => let
      val args =
        list_vt_mapfree_fun<SMTAst><Strptr1>(args, 
                                             lam (smt) => let
                                              val padded = 
                                                strptrlst_concat(
                                                  smt.to_string() :: copy(" ") :: nil)
                                              val () = assertloc(isneqz(padded))
                                             in
                                              padded
                                             end)
      val res = strptrlst_concat(copy("(") :: opr :: copy(" ") 
                                           :: list_vt_extend(args, copy(")")))
      val () = assertloc(isneqz(res))
    in
      res
    end

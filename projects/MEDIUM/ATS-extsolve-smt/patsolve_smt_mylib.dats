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

#define ATS_DYNLOADFLAG 0

(* ****** ****** *)
//
#include
"share/atspre_define.hats"
#include
"share/atspre_staload.hats"
//
(* ****** ****** *)

staload "{$JSONC}/DATS/json.dats"

(* ****** ****** *)
//
local
#include"{$JSONC}/DATS/json_ML.dats"
in (*nothing*) end // end of [local]
//
(* ****** ****** *)

(* end of [patsolve_smt_mylib.dats] *)

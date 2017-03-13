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

#define ATS_DYNLOADFLAG 0

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

staload "{$JSONC}/DATS/json.dats"

(* ****** ****** *)
//
local
#include"{$JSONC}/DATS/json_ML.dats"
#include"{$ARGPARSE}/DATS/argparse.dats"
in (*nothing*) end // end of [local]
//
(* ****** ****** *)

(* end of [patsunj2_mylib.dats] *)

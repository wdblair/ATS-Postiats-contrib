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
#include
"share/atspre_staload.hats"
//
(* ****** ****** *)

#include
"mydepies.hats"

(* ****** ****** *)

staload UN = "prelude/SATS/unsafe.sats"

staload "./patsunj2_commarg.sats"
staload "./patsunj2_parsing.sats"

staload "{$ARGPARSE}/SATS/argparse.sats"

staload "{$JSONC}/SATS/json.sats"
staload "{$JSONC}/SATS/json_ML.sats"

//
(*
dynload "patsunj2_synent2.dats"
*)
val () =
patsunj2_synent2__dynload() where
{
  extern fun patsunj2_synent2__dynload(): void = "ext#"
}
//
(* ****** ****** *)
//
(*
dynload "patsunj2_parsing.dats"
*)
val () =
patsunj2_parsing__dynload() where
{
  extern fun patsunj2_parsing__dynload(): void = "ext#"
}
//
(* ****** ****** *)
//
(*
dynload "patsunj2_commarg.dats"
*)
val () =
patsunj2_commarg__dynload() where
{
  extern fun patsunj2_commarg__dynload(): void = "ext#"
}
//
(* ****** ****** *)

#define ::  list_cons
#define nil list_nil

implement
main0 {n} (argc, argv) =
{
//
val () =
println! ("Hello from [patsunj2]!")
//
val p  = make_parser()
val p' = p.add_param("-i", false)

val results = parse(p', $UN.castvwtp1{arrayref(string,n)}(argv), argc)

val input   = results["-i"]

val js = parse_json_file(input)

val- JSONobject(maps) = js

val- ((_, s2cstjs) :: (_, s2varjs) :: (_, d2conjs) :: (_, d2cstjs) :: (_, d2varjs) :: (_, d2ecljs) :: _) = maps

val ()     = parse_d2varmap(d2varjs)
val d2ecls = parse_d2eclist(d2ecljs)
//
} (* end of [main] *)

(* ****** ****** *)

(* end of [patsunj2_main.dats] *)

(* ****** ****** *)
//
// HX-2014-08:
// A running example
// from ATS2 to Node.js
//
(* ****** ****** *)
//
#include
"share/atspre_define.hats"
//
(* ****** ****** *)
//
staload
"{$LIBATSCC2JS}/basics_js.sats"
staload
"{$LIBATSCC2JS}/SATS/integer.sats"
staload
"{$LIBATSCC2JS}/SATS/string.sats"
//
staload
"{$LIBATSCC2JS}/SATS/node/basics.sats"
//
(* ****** ****** *)
//
#define
ATS_MAINATSFLAG 1
#define
ATS_DYNLOADNAME "fact_dynload"
#define
ATS_STATIC_PREFIX "fact__"
//
(* ****** ****** *)
//
extern
fun fact : int -> int = "mac#fact"
//
implement
fact (n) = if n > 0 then n * fact(n-1) else 1
//
(* ****** ****** *)
//
val N = 10
val () = println! ("fact(", N, ") = ", fact(N))
//
(* ****** ****** *)

%{^
//
// file inclusion
//
var fs = require('fs');
eval(fs.readFileSync('./libatscc2js/CATS/basics_cats.js').toString());
eval(fs.readFileSync('./libatscc2js/CATS/integer_cats.js').toString());
eval(fs.readFileSync('./libatscc2js/CATS/string_cats.js').toString());
eval(fs.readFileSync('./libatscc2js/CATS/node/basics_cats.js').toString());
%} // end of [%{^]

(* ****** ****** *)

%{$
fact_dynload()
%} // end of [%{$]

(* ****** ****** *)

(* end of [fact.dats] *)

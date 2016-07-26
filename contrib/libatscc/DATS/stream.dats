(*
** libatscc-common
*)

(* ****** ****** *)

(*
//
staload "./../SATS/stream.sats"
//
staload UN = "prelude/SATS/unsafe.sats"
//
*)

(* ****** ****** *)

implement
stream_nth_opt
  {a}(xs, n) = let
//
fun
loop
(
  xs: stream(a), n: intGte(0)
) : Option_vt(a) =
(
case+ !xs of
| stream_nil
    () => None_vt()
  // stream_nil
| stream_cons
    (x, xs) =>
  (
    if n > 0
      then loop(xs, pred(n)) else Some_vt(x)
    // end of [if]
  )
) (* end of [loop] *)
//
in
  loop (xs, n)
end // end of [stream_nth_opt]

(* ****** ****** *)

implement
stream_take_opt
  {a}{n}(xs, n) = let
//
fun
auxmain
{i:nat | i <= n}
(
  xs: stream(a), i: int(i), res: list(a, i)
) : Option_vt(list(a, n)) =
(
//
if
(i < n)
then (
  case+ !xs of
  | stream_nil() => None_vt()
  | stream_cons(x, xs) => auxmain(xs, i+1, list_cons(x, res))
) (* end of [then] *)
else Some_vt(list_reverse(res))
//
) (* end of [auxmain] *)
//
in
  auxmain(xs, 0, list_nil())
end // end of [stream_take_opt]

(* ****** ****** *)
//
implement
stream_map_cloref
  (xs, fopr) = $delay
(
//
case+ !xs of
| stream_nil
    ((*void*)) => stream_nil()
  // end of [stream_nil]
| stream_cons(x, xs) =>
    stream_cons (fopr(x), stream_map_cloref(xs, fopr))
  // end of [stream_cons]
//
) (* end of [stream_map_cloref] *)
//
implement
stream_map_method
  (xs, _) = lam(fopr) => stream_map_cloref(xs, fopr)
//
(* ****** ****** *)
//
implement
stream_filter_cloref
  (xs, pred) = $delay
(
//
case+ !xs of
| stream_nil
    ((*void*)) => stream_nil()
  // end of [stream_nil]
| stream_cons
    (x, xs) =>
  (
    if pred(x)
      then
      stream_cons (
        x, stream_filter_cloref(xs, pred)
      ) (* end of [then] *)
      else !(stream_filter_cloref(xs, pred))
    // end of [if]
  ) (* end of [stream_cons] *)
//
) (* end of [stream_filter_cloref] *)
//
implement
stream_filter_method
  (xs) = lam(pred) => stream_filter_cloref(xs, pred)
//
(* ****** ****** *)

implement
stream_tabulate_cloref
  {a}(f) = let
//
fun
aux
(
  n: intGte(0)
) : stream(a) =
(
  $delay(stream_cons(f(n), aux(n+1)))
) (* end of [aux] *)
//
in
  aux(0)
end // end of [stream_tabulate_cloref]

(* ****** ****** *)

implement
stream2cloref_exn
  {a}(xs) = let
//
val rxs =
  ref{stream(a)}(xs)
//
in
//
lam() => let
//
val xs = rxs[]
val-stream_cons(x, xs) = !xs
//
in
  rxs[] := xs; x
end // end of [lam]
//
end // end of [stream2cloref_exn]

(* ****** ****** *)

implement
stream2cloref_opt
  {a}(xs) = let
//
val rxs =
  ref{stream(a)}(xs)
//
in
//
lam() => let
  val xs = rxs[]
in
  case+ !xs of
  | stream_nil() => None_vt()
  | stream_cons(x, xs) => (rxs[] := xs; Some_vt(x))
end // end of [lam]
//
end // end of [stream2cloref_opt]

(* ****** ****** *)

implement
stream2cloref_last
  {a}(xs, x0) = let
//
val rxs =
  ref{stream(a)}(xs)
//
val rx0 = ref{a}(x0)
//
in
//
lam() => let
  val xs = rxs[]
in
  case+ !xs of
  | stream_nil
      () => rx0[]
    // end of [stream_nil]
  | stream_cons
      (x1, xs2) => (rxs[] := xs2; rx0[] := x1; x1)
    // end of [stream_cons]
end // end of [lam]
//
end // end of [stream2cloref]

(* ****** ****** *)

(* end of [stream.dats] *)

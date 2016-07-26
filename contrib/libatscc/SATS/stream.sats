(*
** libatscc-common
*)

(* ****** ****** *)

(*
staload "./../basics.sats"
*)

(* ****** ****** *)
//
fun
stream_nth_opt
  {a:t0p}
(
  xs: stream(INV(a)), n: intGte(0)
) : Option_vt(a) = "mac#%" // end-of-fun
//
(* ****** ****** *)
//
fun
stream_take_opt
  {a:t0p}{n:nat}
(
  xs: stream(INV(a)), n: int(n)
) : Option_vt(list(a,n)) = "mac#%" // end-of-fun
//
fun
stream_drop_opt
  {a:t0p}{n:nat}
(
  xs: stream(INV(a)), n: int(n)
) : Option_vt(stream(a)) = "mac#%" // end-of-fun
//
(* ****** ****** *)
//
fun
stream_map_cloref
  {a:t0p}{b:t0p}
(
  xs: stream(INV(a)), fopr: (a) -<cloref1> b
) : stream(b) = "mac#%" // end-of-function
//
fun
stream_map_method
  {a:t0p}{b:t0p}
(
  xs: stream(INV(a)), TYPE(b))(fopr: (a) -<cloref1> b
) : stream(b) = "mac#%" // end-of-function
//
overload .map with stream_map_method
//
(* ****** ****** *)
//
fun
stream_filter_cloref
  {a:t0p}
(
  xs: stream(INV(a)), pred: (a) -<cloref1> bool
) : stream(a) = "mac#%" // end-of-function
fun
stream_filter_method
  {a:t0p}
(
  xs: stream(INV(a)))(pred: (a) -<cloref1> bool
) : stream(a) = "mac#%" // end-of-function
//
overload .filter with stream_filter_method
//
(* ****** ****** *)
//
fun
stream_tabulate_cloref
  {a:t0p}
  (f: intGte(0) -<cloref1> a): stream(a) = "mac#%"
//
(* ****** ****** *)
//
fun
stream2cloref_exn
  {a:t0p}
  (xs: stream(INV(a))): cfun(a) = "mac#%"
fun
stream2cloref_opt
  {a:t0p}
  (xs: stream(INV(a))): cfun(Option_vt(a)) = "mac#%"
fun
stream2cloref_last
  {a:t0p}(xs: stream(INV(a)), last: a): cfun(a) = "mac#%"
//
(* ****** ****** *)

(* end of [stream.sats] *)

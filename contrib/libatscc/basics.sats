(*
** libatscc-common
*)
(* ****** ****** *)
//
typedef
cfun0 (b:vt0p) = () -<cloref1> b
typedef
cfun1 (a:t0p, b:vt0p) = (a) -<cloref1> b
typedef
cfun2 (a1:t0p, a2:t0p, b:vt0p) = (a1, a2) -<cloref1> b
typedef
cfun3 (a1:t0p, a2:t0p, a3: t0p, b:vt0p) = (a1, a2, a3) -<cloref1> b
//
stadef cfun = cfun0
stadef cfun = cfun1
stadef cfun = cfun2
stadef cfun = cfun3
//
(* ****** ****** *)

(* end of [basics.sats] *)

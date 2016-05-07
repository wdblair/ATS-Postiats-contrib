(*
** Bouncing ball
*)

(* ****** ****** *)
//
// Author: Hongwei Xi
// Authoremail: gmhwxiATgmailDOTedu
//
(* ****** ****** *)
//
staload
"libats/SATS/number/real.sats"
//
staload
"libats/DATS/number/real_double.dats"
//
staload
ModelCheck = "./modelcheck.sats"
//
(* ****** ****** *)

stacst g : real
stacst dt : real
stacst time: real

(* ****** ****** *)

datasort mode = M1 | M2

(* ****** ****** *)

absvtype
state(mode, x:real, v:real)

(* ****** ****** *)
//
extern
fun
state_get_x{m:mode}{x,v:real}(!state(m, x, v)): real(x)
and
state_get_v{m:mode}{x,v:real}(!state(m, x, v)): real(v)
//
overload .x with state_get_x
overload .v with state_get_v
//
(* ****** ****** *)
//
extern
fun
mode1_flow
  : {x,v:real | x > 0}
    state(M1, x, v) ->
    state(M1, x+v*dt, v+g*dt)
//
(* ****** ****** *)
//
extern
fun
mode1_jump
  : {x,v:real | x <= 0}
    state(M1, x, v) -> state(M2, i2r(0), ~v)
//
(* ****** ****** *)
//
extern
fun
mode2_flow
  : {x,v:real | v > 0}
    state(M2, x, v) ->
    state(M2, x+v*dt, v+g*dt)
//
(* ****** ****** *)
//
extern
fun
mode2_jump
  : {x,v:real | v <= 0}
    state(M2, x, v) -> state(M1, x, i2r(0))
//
(* ****** ****** *)

fun
loop1{x,v:real}
  (state: state(M1, x, v)): void = let
  val x = state_get_x(state)
in
  if x > 0
    then loop1(mode1_flow(state)) else loop2(mode1_jump(state))
  // end of [if]
end // end of [loop1]

and
loop2{x,v:real}
  (state: state(M2, x, v)): void = let
  val v = state_get_v(state)
in
  if v > 0
    then loop2(mode2_flow(state)) else loop1(mode2_jump(state))
  // end of [if]
end // end of [loop2]

extern
prfun
make_state {m:mode} (): [x:real][v:real] state(m, x, v)
 
(** Define initial state and the range of possible values for the state *)
prval [x:real] [v:real] state = make_state{M1}()

(** Define range of interest *)
prval () = $ModelCheck.define_range{x}(int2real(0), int2real(20))
prval () = $ModelCheck.define_range{v}(int2real(~100), int2real(100))

prval () = $ModelCheck.initial_value{x}(int2real(10))
prval () = $ModelCheck.initial_value{v}(int2real(0))

(** Bound the amount of time *)
prval () = $ModelCheck.define_range{time}(int2real(0), int2real(10))

(** Is the following state reachable in this model? *)
prval () = $ModelCheck.add_goal{x == i2r(10) && v > i2r(10)}()

(* ****** ****** *)

(* end of [bouncing_ball.dats] *)

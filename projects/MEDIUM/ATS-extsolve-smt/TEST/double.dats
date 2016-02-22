#include "share/atspre_staload.hats"

staload UN = "prelude/SATS/unsafe.sats"

staload "float.sats"
staload "double.sats"

#define
PI 3.1415926536
//
//macdef _fp64(x) = $UN.cast{double(fp64(,(x)))}(,(x))
//
val PI_ =
  $UN.cast{double(fp64(PI))}(PI)
//
val PI_sqrt = sqrt_double(PI_)
//
(**
An example of a simple decision to turn the plane left or right

Uses Frama-C along with Coq to prove its accuracy

//@ logic integer l_sign(real x) = (x >= 0.0) ? 1 : -1;

/*@ requires e1<= x-\exact(x) <= e2;   
  @ ensures  (\result != 0 ==> \result == l_sign(\exact(x))) &&
  @          \abs(\result) <= 1 ;
  @*/
int sign(double x, double e1, double e2) {

  if (x > e2)
    return 1;
  if (x < e1)
    return -1;
  return 0;
}

*)

stacst sgn_float64: (float64) -> int
stadef sign = sgn_float64

stacst gte_float_float : (float, float) -> bool
stadef >= = gte_float_float

stacst lt_float_float : (float, float) -> bool
stadef < = lt_float_float

fun 
sign {x:float64} (d: double(x)): int(sign(x)) =
  if d >= $UN.cast{double(fp64(0.0))}(0.0) then
    1
  else
    ~1
    
fun
eps_line {sx,sy,vx,vy:float64 | abs(sx) <= fp64(100.0);
                                abs(sy) <= fp64(100.0);
                                abs(vx) <= fp64(1.0);
                                abs(vy) <= fp64(1.0)} (
  sx: double(sx), sy: double(sy),
  vx: double(vx), vy: double(vy)
): int(sign(sx*vx + sy*vy)*sign(sx*vy - sy*vx)) = let
  val s1 = sign(sx*vx + sy*vy)
  val s2 = sign(sx*vy - sy*vx)
in
 s1*s2
end

(**
*)
datasort real = (** *)

(**

Suppose your aircraft's position is given by sx, sy and your velocity
is given by vox, voy.

My first attempt at using floats in the statics. I want to specify
that the heading and velocity produced by this function is _safe_ with
respect to the intruding aircraft's velocity (given by (vix,viy)).
This type of safety (the safety of the system) is a little beyond
the scope of what I want.

Instead, these constraints should be expressed using real numbers since
that is the domain they are reasoned in.

When we actually implement the function, we want to prove that the
result is actually within some bound of the "true" answer. That is,
the heading is defined as some function of our and the intruder's
position. I want to prove that whatever method I use to compute that
value, the error between the floating point number I return and its
true value stated in the function's specification has an acceptable
bound.

fun
update_heading {sx,sy,vox,voy,vix,viy,epsilon: float64} (
  sx: double(sx), sy: double(sy), 
  vox: double(vox), voy: double(voy),
  vix: double(vix), viy: double(viy)
): [heading,vx,vy:float64 | heading >= neg(fp64(180.0)); heading <= fp64(180.0)] (
  double(heading), double(vx), double(vy)
) = let
in
  ($UN.cast{double(fp64(0.0))}(0.0), $UN.cast{double(fp64(0.0))}(0.0), $UN.cast{double(fp64(0.0))}(0.0))
end

The Paper "What Every Computer Scientist Should Know About Floating
Point Arithmetic" is a great resource for examples.

*)

fun
quadratic_formula (
  b: float, a: float, c: float
): float = let
  val b2 = b*b
  val ac = 4 * a * c
in
  b2 - ac
end

implement
main0 (argc, argv) = {
  val r = quadratic_formula (3.34, 1.22, 2.28)
  val () = println! ("Answer: ", r)
}
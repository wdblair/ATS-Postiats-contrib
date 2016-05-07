(** 
  An interface to support integrating ATS with the dReach model checker.
*)
staload
"libats/SATS/NUMBER/real.sats"
//
staload
"libats/DATS/NUMBER/real_double.dats"

prfun
define_range: {x:real} {lower,upper:real}
 (real(lower), real(upper)) -<> [lower <= x; x <= upper] void
 
prfun
initial_value: {x:real} {v: real}
 (real(v)) -<> [x == v] void

prfun
add_goal: {b:bool}
 () -<> [b] void
(*
  Powertrain
*)

(* ****** ****** *)
//
// Author: Will Blair
// Authoremail: wdblairATbuDOTedu
//
(* ****** ****** *)
//
staload
"libats/SATS/NUMBER/real.sats"
//
staload
"libats/DATS/NUMBER/real_double.dats"
//
(* ****** ****** *)

#define c1	f2r(0.41328)   // page 262 (table 3)
#define c2	 ~f2r(0.366)    // page 262 (table 3)
#define c3	f2r(0.08979)   // page 262 (table 3)
#define c4	~f2r(0.0337)   // page 262 (table 3)
#define c5	 f2r(0.0001)    // page 262 (table 3)
#define c6	  f2r(2.821)     // page 262 (table 3)
#define c7     ~f2r(0.05231)  // page 262 (table 3)
#define c8	f2r(0.10299)   // page 262 (table 3)
#define c9     ~f2r(0.00063)  // page 262 (table 3)
#define c10	    f2r(1.0)       // page 262 (table 3)
#define c11	   f2r(14.7)      // page 262 (table 3)  //??
#define c11P	   f2r(12.5)      // page 262 (table 3)  //??
#define c12	    f2r(0.9)       // page 262 (table 5)
#define c13	   f2r(0.04)      // page 262 (table 3)
#define c14	   f2r(0.14)      // page 262 (table 3)
#define c15	 f2r(13.893)    // page 262 (table 5)
#define c16    ~f2r(35.2518)  // page 262 (table 5)
#define c17	f2r(20.7364)   // page 262 (table 5)
#define c18	 f2r(2.6287)    // page 262 (table 5)
#define c19	 ~f2r(1.592)    // page 262 (table 5)
#define c20	~f2r(2.3421)   // page 262 (table 5)
#define c21      f2r(2.7799)    // page 262 (table 5)
#define c22	~f2r(0.3273)   // page 262 (table 5)
#define c23	    f2r(1.0)       // page 262 (table 5)
#define c24	    f2r(1.0)       // page 262 (table 5)
#define c25	    f2r(1.0)       // page 262 (table 5)
#define c26	    f2r(4.0)       // page 262 (table 5)

(* ****** ****** *)

#define tauI	10  //
#define h	10  //

(* ****** ****** *)

#define thetaHat   (c6 + c7*theta + c8*theta*theta + c9*theta*theta*theta) // page 256 (throttle air dynamics)
#define dmAf       (i2r(2) * thetaHat * sqrt((p/c10) - (p/c10)**i2r(2)))  // page 256, (2)
#define mCf(x)     (c2 + c3*w*x + c4*w*x*x + c5*w*w*x)
#define dmC        (c12 * mCf(p))  // page 256, (3)

(* ****** ****** *)

#define thetaI_init 80
#define thetaI_amp 10

(* ****** ****** *)

%{^
/**
dReach {
[0,180] theta;
[0,10]   p;
[0,100] lambda;

[0,10]   pe;
[0,100] i;
[0,100] tau;
[0,10]  fc;

[0,180] thetaI;
[0,150] w;

[0,h]   t;
[0,h]   time;
}
*/
%}

stacst time: real
stacst dt: real

(* ****** ****** *)

datasort mode = Startup | Normal | Power | SensorFail

(* ****** ****** *)

absvtype
EngineState (
 mode,
 theta: real,
 p: real,
 lambda: real,
 pe: real,
 i: real,
 tau: real,
 fc: real,
 thetaI: real,
 w: real,
 t: real
)

(* ****** ****** *)

stadef d_dt(x, y) = x + y * dt

(* ****** ****** *)

extern
fun
state_get_theta {m:mode} {theta,p,lambda,pe,i,tau,fc,thetaI,w,t:real} (
  !EngineState(m, theta, p, lambda, pe, i, tau, fc, thetaI, w, t)
): real(theta)

extern
fun
state_get_p {m:mode} {theta,p,lambda,pe,i,tau,fc,thetaI,w,t:real} (
  !EngineState(m, theta, p, lambda, pe, i, tau, fc, thetaI, w, t)
): real(p) 

extern
fun
state_get_t {m:mode} {theta,p,lambda,pe,i,tau,fc,thetaI,w,t:real} (
  !EngineState(m, theta, p, lambda, pe, i, tau, fc, thetaI, w, t)
): real(t)

extern
fun
state_get_tau {m:mode} {theta,p,lambda,pe,i,tau,fc,thetaI,w,t:real} (
  !EngineState(m, theta, p, lambda, pe, i, tau, fc, thetaI, w, t)
): real(tau)

overload .theta with state_get_theta
overload .p with state_get_p
overload .t with state_get_t
overload .tau with state_get_tau

(* ****** ****** *)

val timer_high = int2real(h)

(* ****** ****** *)

// Startup

extern
fun
startup_flow : {theta,p,lambda,pe,i,tau,fc,thetaI,w,t:real | t >= i2r(0)}
  EngineState(Startup, theta, p, lambda, pe, i, tau, fc, thetaI, w, t) -> 
    EngineState(Startup,
      d_dt(theta, i2r(10) * (thetaI - theta)),
      d_dt(p,     c1 * (dmAf - dmC)),
      d_dt(lambda, c26 * (c15 + c16*c25*fc + c17*c25*c25*fc*fc + c18*dmC + c19*dmC*c25*fc - lambda)),
      pe, i, tau, fc, thetaI, w, d_dt(t, ~i2r(1))
    )

extern
fun
startup_timer_overflow : {theta,p,lambda,pe,i,tau,fc,thetaI,w,t:real | t == i2r(h)}
  EngineState(Startup, theta, p, lambda, pe, i, tau, fc, thetaI, w, t) -> 
   EngineState(
    Startup, 
    theta, p, lambda, thetaI, w, pe, 
    i2r(0), tau + i2r(h), mCf(pe) / c11, i2r(h)
   )
   
extern
fun
startup_goto_normal : {theta,p,lambda,pe,i,tau,fc,thetaI,w,t:real | tau >= i2r(tauI)}
  EngineState(Startup, theta, p, lambda, pe, i, tau, fc, thetaI, w, t) -> 
   EngineState(Normal, theta, p, lambda, thetaI, w, pe, i, tau, mCf(pe) / c11, i2r(h))

(* ****** ****** *)

// Normal






(**
  The definition of slist is exported to the SMT solver.
  If you use CVC4 as your back end, then it can solve
  some constraints that require inductive reasoning.
*)
datasort slist =
 | snil of ()
 | scons of (int, slist)

stacst empty : (slist) -> bool
stacst head : (slist) -> int
stacst tail : (slist) -> int

abst@ype T(a:t@ype, x: int) = a

datatype list(a:t@ype, xs:slist) =
  | nil (a, snil()) of ()
  | {x:int} {xss:slist}
    cons (a, scons(x, xss)) of (T(a,x), list(a, xss))
    
extern
fun {a:t@ype} wrap_val (a): [x:int] T(a, x)

extern
fun {a:t@ype} head {sxs:slist | ~empty(sxs)} (
  xs: list(a, sxs)
): T(a, head(sxs))

extern
fun {a:t@ype} tail {sxs:slist} (
  xs: list(a, sxs)
): T(a, tail(sxs))

implement main0 () = {
  val xs = cons(wrap_val(20), cons(wrap_val(10), nil()))
  val top = head(xs)
}
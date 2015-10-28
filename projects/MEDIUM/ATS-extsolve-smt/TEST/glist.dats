(**
  The definition of slist is exported to the SMT solver.
  If you use CVC4 as your back end, then it can solve
  some constraints that require inductive reasoning.
*)
datasort Nat =
  | zero of ()
  | succ of (Nat)

datasort slist =
 | snil of ()
 | scons of (Nat, slist)
 
stacst empty : (slist) -> bool
stacst head : (slist) -> Nat
stacst tail : (slist) -> Nat

stacst append : (slist, slist) -> slist
stacst take : (slist, Nat) -> slist
stacst drop : (slist, Nat) -> slist

abst@ype T(a:t@ype, x: Nat) = a

datatype list(a:t@ype, xs:slist) =
  | nil (a, snil()) of ()
  | {x:Nat} {xss:slist}
    cons (a, scons(x, xss)) of (T(a,x), list(a, xss))
    
extern
fun {a:t@ype} wrap_val (a): [x:Nat] T(a, x)

extern
fun {a:t@ype} head {sxs:slist | ~empty(sxs)} (
  xs: list(a, sxs)
): T(a, head(sxs))

extern
fun {a:t@ype} tail {sxs:slist} (
  xs: list(a, sxs)
): T(a, tail(sxs))

extern
praxi
empty_base_lemma
  (): [empty(snil()) == true] unit_p

extern
praxi
empty_ind_lemma {x:Nat} {xs:slist}
  (): [empty(scons(x,xs)) == false] unit_p

extern
praxi
append_base_lemma {xs:slist}
  (): [append(snil(), xs) == xs] unit_p
  
extern
praxi
append_ind_lemma {x:Nat} {xs,ys:slist}
  (): [append(scons(x, xs), ys) == scons(x, append(xs,ys))] unit_p

extern
praxi
take_base_lemma {xs:slist}
  (): [take(xs, zero()) == snil()] unit_p
  
extern
praxi
take_nil_lemma {n:Nat} {xs:slist}
  (): [take(snil(), n) == snil()] unit_p  
  
extern
praxi
take_ind_lemma {x,n:Nat} {xs:slist}
  (): [take(scons(x, xs), succ(n)) == scons(x, take(xs, n))] unit_p
  
extern
praxi
drop_base_lemma {xs:slist}
  (): [drop(xs, zero()) == xs] unit_p
  
extern
praxi
drop_nil_lemma {xs:slist}
  (): [drop(snil(), zero()) == xs] unit_p
  
extern
praxi
drop_ind_lemma {x,n:Nat} {xs:slist}
  (): [drop(scons(x, xs), succ(n)) == drop(xs, n)] unit_p

(** CVC4 can prove this automatically *)
prfun
append_lemma {n:Nat} {xs:slist} .<>.
  (): [append(take(xs,n), drop(xs,n)) == xs] unit_p = let
  prval () = $solver_assert(take_base_lemma)
  prval () = $solver_assert(take_nil_lemma)
  prval () = $solver_assert(take_ind_lemma)
  //
  prval () = $solver_assert(drop_base_lemma)
  prval () = $solver_assert(drop_nil_lemma)
  prval () = $solver_assert(drop_ind_lemma)
  //
  prval () = $solver_assert(append_base_lemma)
  prval () = $solver_assert(append_ind_lemma)
in
  unit_p ()
end

implement main0 () = {
  prval () = $solver_assert(empty_base_lemma)
  prval () = $solver_assert(empty_ind_lemma)
  val xs = cons(wrap_val(20), cons(wrap_val(10), nil()))
  val top = head(xs)
}
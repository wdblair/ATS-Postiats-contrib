(*
##
## ATS-extsolve-smt:
## Solving ATS-constraints with arbitrary solvers
##
*)

(* ****** ****** *)
//
#ifndef PATSOLVE_SMT_SOLVING
#include "./myheader.hats"
#endif // end of [ifndef]
//
(* ****** ****** *)
//
staload UN = "prelude/SATS/unsafe.sats"
//


#define :: cons_vt
#define nil nil_vt

implement plugin_bag () = () where {

	val _ = println! "(declare-sort BagElt 0)"
	val _ = println! "(define-sort Bag () (Array BagElt Int))"

	val _ = println! "(define-fun bag_emp () Bag ((as const Bag) 0))"
	val _ = println! "(define-fun bag_mem ((s Bag) (x BagElt)) Bool (> (select s x) 0))"
	val _ = println! "(define-fun bag_add ((s Bag) (x BagElt)) Bag  (store s x (+ (select s x) 1)))"
	val _ = println! "(define-fun bag_del ((s Bag) (x BagElt)) Bag  (store s x (ite (> (select s x) 0) (- (select s x) 1) 0)))"
	val _ = println! "(define-fun bag_rmv ((s Bag) (x BagElt)) Bag  (store s x 0))"

	val _ = println! "(declare-fun bag_fun_del (Int Int) Int)"
	val _ = println! "(assert (forall ((x Int) (y Int)) (= (bag_fun_del x y) (ite (>= x y) (- x y) 0))))"
	val _ = println! "(define-fun bag_cup ((s1 Bag) (s2 Bag)) Bag ((_ map (+ (Int Int) Int)) s1 s2))"
	val _ = println! "(define-fun bag_dif ((s1 Bag) (s2 Bag)) Bag ((_ map bag_fun_del) s1 s2))"

	val _ = println! "(declare-fun bag_fun_min (Int Int) Int)"
	val _ = println! "(declare-fun bag_fun_max (Int Int) Int)"
	val _ = println! "(assert (forall ((x Int) (y Int)) (= (bag_fun_min x y) (ite (> x y) y x))))"
	val _ = println! "(assert (forall ((x Int) (y Int)) (= (bag_fun_max x y) (ite (< x y) y x))))"
	val _ = println! "(define-fun bag_cap ((s1 Bag) (s2 Bag)) Bag ((_ map bag_fun_min) s1 s2))"
	val _ = println! "(define-fun bag_jon ((s1 Bag) (s2 Bag)) Bag ((_ map bag_fun_max) s1 s2))"

	val _ = println! "(define-fun bag_sub ((s1 Bag) (s2 Bag)) Bool (= bag_emp (bag_dif s2 s1)))"
	val _ = println! "(define-fun bag_eq  ((s1 Bag) (s2 Bag)) Bool (= s1 s2))"

	val _ = println! "(define-fun bag_car ((s Bag) (x BagElt)) Int (select s x))"

//	val _ = println! "(declare-fun bag_fun_1 (Int) Int)"
//	val _ = println! "(assert (forall ((x Int)) (= (bag_fun_1 x) (ite (>= x 1) 1 0))))"
//	val _ = println! "(define-fun bag_set ((s Bag)) Bag ((_ map bag_fun_1) s))"

}

////
implement sort_bag () = Atom(copy("Bag"))
implement sort_bag_elt () = Atom(copy("BagElt"))

fun formula_bag_empty (): form 
fun formula_bag_add (form, form): form 
fun formula_bag_del (form, form): form 
fun formula_bag_union (form, form): form 
fun formula_bag_diff (form, form): form
fun formula_bag_join (form, form): form
fun formula_bag_intersect (form, form): form 
fun formula_bag_member (form, form): form 
fun formula_bag_eq (form, form): form 
fun formula_bag_cardinality (form, form): form
fun formula_bag_remove (form, form): form

implement formula_bag_remove (s, e) = res where {
  val opr = string0_copy "bag_rmv"
  val res = Apply (opr, s :: e :: nil)
}

implement formula_bag_cardinality (s, e) = res where {
  val opr = string0_copy "bag_car"
  val res = Apply (opr, s :: e :: nil)
} 

implement formula_bag_empty () = res where {
  val opr = string0_copy "bag_emp"
  val res = Atom (opr)
} 
implement formula_bag_add (s, e) = res where {
  val opr = string0_copy "bag_add"
  val res = Apply (opr, s :: e :: nil)
} 
implement formula_bag_del (s, e) = res where {
  val opr = string0_copy "bag_del"
  val res = Apply (opr, s :: e :: nil)
} 
implement formula_bag_union (s1, s2) = res where {
  val opr = string0_copy "bag_cup"
  val res = Apply (opr, s1 :: s2 :: nil)
} 
implement formula_bag_diff (s1, s2) = res where {
  val opr = string0_copy "bag_dif"
  val res = Apply (opr, s1 :: s2 :: nil)
}
implement formula_bag_join (s1, s2) = res where {
  val opr = string0_copy "bag_jon"
  val res = Apply (opr, s1 :: s2 :: nil)
}
implement formula_bag_intersect (s1, s2) = res where {
  val opr = string0_copy "bag_cap"
  val res = Apply (opr, s1 :: s2 :: nil)
} 
implement formula_bag_member (s, e) = res where {
  val opr = string0_copy "bag_mem"
  val res = Apply (opr, s :: e :: nil)
} 
implement formula_bag_eq (s1, s2) = res where {
  val opr = string0_copy "bag_eq"
  val res = Apply (opr, s1 :: s2 :: nil)
} 

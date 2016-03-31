(*
##
## ATS-extsolve-smt:
## Solving ATS-constraints with SMT
##
*)

(* ****** ****** *)
//
#ifndef
PATSOLVE_SMT_SOLVING
#include "./myheader.hats"
#endif // end of [ifndef]
//
(* ****** ****** *)
//
staload
UN = "prelude/SATS/unsafe.sats"
//
(* ****** ****** *)
//
implement
print_s2cinterp
  (x) = fprint_s2cinterp(stdout_ref, x)
implement
prerr_s2cinterp
  (x) = fprint_s2cinterp(stderr_ref, x)
//
implement
fprint_s2cinterp
  (out, x) = let
in
//
case+ x of
//
| S2CINTnone() => fprint! (out, "S2CINTnone()")
//
| S2CINTsome _ => fprint! (out, "S2CINTsome(...)")
//
| S2CINTbuiltin_0 _ => fprint! (out, "S2CINTbuiltin_0(...)")
| S2CINTbuiltin_1 _ => fprint! (out, "S2CINTbuiltin_1(...)")
| S2CINTbuiltin_2 _ => fprint! (out, "S2CINTbuiltin_2(...)")
//
| S2CINTbuiltin_list _ => fprint! (out, "S2CINTbuiltin_list(...)")
//
end // end of [fprint_s2cinterp]
//
(* ****** ****** *)

implement
fprint_val<s2cinterp> = fprint_s2cinterp

(* ****** ****** *)

local

typedef
key = string and itm = s2cinterp

in (* in-of-local *)

#include "libats/ML/HATS/myhashtblref.hats"

end // end of [local]

(* ****** ****** *)

local
//
val
the_s2cinterp_map = myhashtbl_make_nil(1024)
//
fun
s2cinterp_insert
(
  name: string, itm: s2cinterp
) : void =
{
//
val-~None_vt() =
  myhashtbl_insert(the_s2cinterp_map, name, itm)
// end of [val]
} (* end of [s2cinterp_insert] *)
//
in (* in-of-local *)
//
implement
s2cst_get_s2cinterp
  (s2c) = let
//
val ptr = s2cst_get_payload(s2c)
//
in
//
if
ptr > 0
then $UN.cast{s2cinterp}(ptr)
else let
//
val key = symbol_get_name(s2c.name())
val opt = myhashtbl_search(the_s2cinterp_map, key)
val itm =
(
  case+ opt of
  | ~Some_vt(itm) => itm
  | ~None_vt((*void*)) => let
  in
    S2CINTnone()
  end
) : s2cinterp // end of [val]
val () = s2cst_set_payload(s2c, $UN.cast{ptr}(itm))
//
in
  itm
end // end of [else]
//
end // end of [s2cst_get_s2cinterp]
//
implement
the_s2cinterp_initize() =
{
//
macdef insert = s2cinterp_insert
//
val () = insert("null_addr", S2CINTbuiltin_0(formula_null))
//
val () = insert("true_bool", S2CINTbuiltin_0(formula_true))
val () = insert("false_bool", S2CINTbuiltin_0(formula_false))
//
val () = insert("neg_int", S2CINTbuiltin_1(formula_ineg))
//
val () = insert("add_int_int", S2CINTbuiltin_2(formula_iadd))
val () = insert("sub_int_int", S2CINTbuiltin_2(formula_isub))
//
val () = insert("mul_int_int", S2CINTbuiltin_2(formula_imul))
val () = insert("div_int_int", S2CINTbuiltin_2(formula_idiv))
val () = insert("idiv_int_int", S2CINTbuiltin_2(formula_idiv))
val () = insert("ndiv_int_int", S2CINTbuiltin_2(formula_ndiv))
//
val () = insert("lt_int_int", S2CINTbuiltin_2(formula_ilt))
val () = insert("lte_int_int", S2CINTbuiltin_2(formula_ilte))
val () = insert("gt_int_int", S2CINTbuiltin_2(formula_igt))
val () = insert("gte_int_int", S2CINTbuiltin_2(formula_igte))
val () = insert("eq_int_int", S2CINTbuiltin_2(formula_ieq))
val () = insert("neq_int_int", S2CINTbuiltin_2(formula_ineq))
//
val () = insert("abs_int", S2CINTbuiltin_1(formula_iabs))
val () = insert("sgn_int", S2CINTbuiltin_1(formula_isgn))
//
val () = insert("max_int_int", S2CINTbuiltin_2(formula_imax))
val () = insert("min_int_int", S2CINTbuiltin_2(formula_imin))
//
val () = insert("neg_bool", S2CINTbuiltin_1(formula_bneg))
//
val () = insert("add_bool_bool", S2CINTbuiltin_2(formula_badd))
val () = insert("mul_bool_bool", S2CINTbuiltin_2(formula_bmul))
//
val () = insert("lt_bool_bool", S2CINTbuiltin_2(formula_blt))
val () = insert("lte_bool_bool", S2CINTbuiltin_2(formula_blte))
val () = insert("gt_bool_bool", S2CINTbuiltin_2(formula_bgt))
val () = insert("gte_bool_bool", S2CINTbuiltin_2(formula_bgte))
val () = insert("eq_bool_bool", S2CINTbuiltin_2(formula_beq))
val () = insert("neq_bool_bool", S2CINTbuiltin_2(formula_bneq))
//
val () = insert("abs_float64", S2CINTbuiltin_1(formula_fpabs))
val () = insert("neg_float64", S2CINTbuiltin_1(formula_fpneg))
val () = insert("sqrt_float64", S2CINTbuiltin_1(formula_fpsqrt))
//
val () = insert("add_float64_float64", S2CINTbuiltin_2(formula_fpadd))
val () = insert("sub_float64_float64", S2CINTbuiltin_2(formula_fpsub))
//
val () = insert("mul_float64_float64", S2CINTbuiltin_2(formula_fpmul))
val () = insert("div_float64_float64", S2CINTbuiltin_2(formula_fpdiv))
val () = insert("rem_float64_float64", S2CINTbuiltin_2(formula_fprem))
val () = insert("fma_float64_float64", S2CINTbuiltin_list(formula_fpfma))
//
val () = insert("lt_float64_float64", S2CINTbuiltin_2(formula_fplt))
val () = insert("lte_float64_float64", S2CINTbuiltin_2(formula_fplte))
val () = insert("gt_float64_float64", S2CINTbuiltin_2(formula_fpgt))
val () = insert("gte_float64_float64", S2CINTbuiltin_2(formula_fpgte))
val () = insert("eq_float64_float64", S2CINTbuiltin_2(formula_fpeq))
val () = insert("neq_float64_float64", S2CINTbuiltin_2(formula_fpneq))
//
val () = insert("float64_round", S2CINTbuiltin_1(formula_fpround))
//
val () = insert("float64_is_normal", S2CINTbuiltin_1(formula_fp_is_normal))
val () = insert("float64_is_subnormal", S2CINTbuiltin_1(formula_fp_is_subnormal))
val () = insert("float64_is_zero", S2CINTbuiltin_1(formula_fp_is_zero))
val () = insert("float64_is_infinite", S2CINTbuiltin_1(formula_fp_is_infinite))
val () = insert("float64_is_nan", S2CINTbuiltin_1(formula_fp_is_nan))
val () = insert("float64_is_negative", S2CINTbuiltin_1(formula_fp_is_negative))
val () = insert("float64_is_positive", S2CINTbuiltin_1(formula_fp_is_positive))
//
val () = insert("float_to_float64", S2CINTbuiltin_1(formula_float_to_fp64))
//
val () = insert("max_float64_float64", S2CINTbuiltin_2(formula_fpmax))
val () = insert("min_float64_float64", S2CINTbuiltin_2(formula_fpmin))
//
val () = insert("add_addr_int", S2CINTbuiltin_2(formula_iadd))
val () = insert("sub_addr_int", S2CINTbuiltin_2(formula_isub))
val () = insert("sub_addr_addr", S2CINTbuiltin_2(formula_isub))
//
val () = insert("lt_addr_addr", S2CINTbuiltin_2(formula_ilt))
val () = insert("lte_addr_addr", S2CINTbuiltin_2(formula_ilte))
val () = insert("gt_addr_addr", S2CINTbuiltin_2(formula_igt))
val () = insert("gte_addr_addr", S2CINTbuiltin_2(formula_igte))
val () = insert("eq_addr_addr", S2CINTbuiltin_2(formula_ieq))
val () = insert("neq_addr_addr", S2CINTbuiltin_2(formula_ineq))
(** Not very precise, but makes sense in smt... *)
val () = insert("==", S2CINTbuiltin_2(formula_eqeq))

val () = insert("bag_emp", S2CINTbuiltin_0(formula_bag_empty))
val () = insert("bag_add", S2CINTbuiltin_2(formula_bag_add))
val () = insert("bag_del", S2CINTbuiltin_2(formula_bag_del))
val () = insert("bag_cup", S2CINTbuiltin_2(formula_bag_union))
val () = insert("bag_dif", S2CINTbuiltin_2(formula_bag_diff))
val () = insert("bag_jon", S2CINTbuiltin_2(formula_bag_join))
val () = insert("bag_cap", S2CINTbuiltin_2(formula_bag_intersect))
val () = insert("bag_mem", S2CINTbuiltin_2(formula_bag_member))
val () = insert("bag_eq", S2CINTbuiltin_2(formula_bag_eq))
val () = insert("bag_car", S2CINTbuiltin_2(formula_bag_cardinality))
val () = insert("bag_rmv", S2CINTbuiltin_2(formula_bag_remove))
//
(*
val () = insert("atsvoid_prop", S2CINTbuiltin_0(formula_true))
*)
//
} (* end of [the_s2cinterp_initize] *)

end // end of [local]

(* ****** ****** *)

(* end of [patsolve_smt_solving_interp.dats] *)
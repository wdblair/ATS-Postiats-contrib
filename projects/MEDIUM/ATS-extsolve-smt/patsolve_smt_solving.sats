(*
##
## ATS-extsolve-smt:
## Solving ATS-constraints with SMT
##
*)

(* ****** ****** *)
//
#define
ATS_PACKNAME "PATSOLVE_SMT_SOLVING"
//
(* ****** ****** *)
//
#define
PATSOLVE_targetloc "./.ATS-extsolve"
//
(* ****** ****** *)
//
staload "{$PATSOLVE}/patsolve_cnstrnt.sats"
//
(* ****** ****** *)
//
fun c3nstr_smt_solve(c3nstr): void
//
(* ****** ****** *)
//
absvtype sort_vtype = ptr
//
vtypedef sort = sort_vtype
vtypedef sortlst = List0_vt (sort)
//
absvtype form_vtype = ptr
//
vtypedef form = form_vtype
vtypedef formlst = List0_vt (form)
//
absvtype func_decl_vtype = ptr
vtypedef func_decl = func_decl_vtype
//
(* ****** ****** *)
//
datavtype Ast(a:vt@ype+) =
  | Atom of (a)
  | Apply of (a, List0_vt(Ast(a)))
  
vtypedef SMTAst = Ast(Strptr1)
//
fun
smtlib_to_string(SMTAst): Strptr1

overload .to_string with smtlib_to_string
//
(* ****** ****** *)
//
typedef lbool = int
//
macdef ltrue = 1
macdef undef = 0
macdef lfalse = ~1
//
(* ****** ****** *)
//
fun sort_decref (sort): void
fun sort_incref (!sort): sort
//
fun formula_decref (form): void
fun formula_incref (!form): form
//
overload .decref with formula_decref
overload .incref with formula_incref
//
(* ****** ****** *)
//
fun sort_int (): sort
fun sort_bool (): sort
//
fun sort_real (): sort
(**
  In SMT-LIB Floats are parameterized by their
  size, so we can have two separate sorts
*)
fun sort_float (): sort
//
fun sort_bv (int): sort
//
fun sort_fp (): sort
(*
fun sort_string (): sort
*)


(* ****** ****** *)

fun sort_mk_cls (): sort
fun sort_mk_eff (): sort

(* ****** ****** *)
//
fun sort_mk_type (): sort
fun sort_mk_vtype (): sort
//
fun sort_mk_t0ype (): sort
fun sort_mk_vt0ype (): sort
//
fun sort_mk_prop (): sort
fun sort_mk_view (): sort
//
fun sort_mk_tkind (): sort
//
(* ****** ****** *)
//
fun sort_declare_abstract (name: string): void
//
fun sort_declare_alias (alias: string, sort: string): void
//
fun sort_mk_abstract (name: string): sort
//
(* ****** ****** *)
//
fun sort_error (s2rt): sort
//
(* ****** ****** *)
//
fun sort_make_s2rt (s2rt): sort
//
fun sort_declare_s2rtdat (s2rtdat): void
//
(* ****** ****** *)
//
fun sort_to_smtlib (sort): SMTAst
//
fun formula_null (): form

fun formula_true (): form
fun formula_false (): form

fun formula_unit_p (): form

(* ****** ****** *)

fun formula_int (i: int): form
fun formula_intrep (rep: string): form

(* ****** ****** *)
//
fun formula_not (form): form
fun formula_disj (form, form): form
fun formula_conj (form, form): form
fun formula_impl (form, form): form
//
(* ****** ****** *)

fun formula_conj_list (formlst): form
fun formula_conj_list1 (formlst, form): form
fun formula_impl_list1 (formlst, form): form

(* ****** ****** *)
//
fun formula_ineg (form): form
//
fun formula_iadd (form, form): form
fun formula_isub (form, form): form
//
fun formula_imul (form, form): form
//
fun formula_idiv (form, form): form
fun formula_ndiv (form, form): form
//
fun formula_ilt (form, form): form
fun formula_ilte (form, form): form
fun formula_igt (form, form): form
fun formula_igte (form, form): form
fun formula_ieq (form, form): form
fun formula_ineq (form, form): form
//
(* ****** ****** *)
//
fun formula_fpabs (form): form
//
fun formula_fpneg (form): form
//
fun formula_fpadd (form, form): form
fun formula_fpsub (form, form): form
//
fun formula_fpmul (form, form): form
//
fun formula_fpdiv (form, form): form
//
(** fused multiple and addition *)
val formula_fpfma (formlst):<cloref1> form
//
fun formula_fpsqrt (form): form
fun formula_fprem (form, form): form
//
fun formula_fpround (form): form
//
fun formula_fpmin (form, form): form
fun formula_fpmax (form, form): form
//
fun formula_fplt (form, form): form
fun formula_fplte (form, form): form
fun formula_fpgt (form, form): form
fun formula_fpgte (form, form): form
fun formula_fpeq (form, form): form
fun formula_fpneq (form, form): form
//
fun formula_fp_is_normal (form): form
fun formula_fp_is_subnormal (form): form
fun formula_fp_is_zero (form): form
fun formula_fp_is_infinite (form): form
fun formula_fp_is_nan (form): form
fun formula_fp_is_negative (form): form
fun formula_fp_is_positive (form): form
//
fun formula_float_to_fp64 (form): form
//
(* ****** ****** *)
//
fun formula_iabs (form): form
//
fun formula_isgn (form): form
//
fun formula_imax (form, form): form
fun formula_imin (form, form): form
//
(* ****** ****** *)
//
fun formula_bneg (form): form
//
fun formula_badd (form, form): form
fun formula_bmul (form, form): form
//
fun formula_blt (form, form): form
fun formula_blte (form, form): form
fun formula_bgt (form, form): form
fun formula_bgte (form, form): form
fun formula_beq (form, form): form
fun formula_bneq (form, form): form

(* ****** ****** *)
(* custom support for multisets/bags *)
(* ****** ****** *)

fun plugin_bag (): void // prints out the theory of bag 

fun sort_bag (): sort 
fun sort_bag_elt (): sort 

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
(* ****** ****** *)

(* ****** ****** *)
(* custom support for set *)
(* ****** ****** *)
fun plugin_set (): void // prints out the theory of bag 

fun sort_set (): sort 
fun sort_set_elt (): sort 

fun formula_set_empty (): form 
fun formula_set_add (form, form): form 
fun formula_set_del (form, form): form 
fun formula_set_union (form, form): form 
fun formula_set_diff (form, form): form
fun formula_set_intersect (form, form): form 
fun formula_set_member (form, form): form 
fun formula_set_eq (form, form): form 
(* ****** ****** *)

(* ****** ****** *)
(* custom support for list *)
(* ****** ****** *)
fun plugin_list (): void 

fun sort_list (): sort 
fun sort_list_elt (): sort 

fun formula_list_hd (form): form
fun formula_list_tl (form): form
fun formula_list_nil (): form
fun formula_list_cons (form, form): form
fun formula_list_eq (form, form): form

//
(* ****** ****** *)
//
fun
formula_cond
(
  f_cond: form, f_then: form, f_else: form
) : form // end of [formula_cond]
//
(* ****** ****** *)
//
fun
formula_eqeq (s2e1: form, s2e2: form): form
//
(* ****** ****** *)

fun
formula_sizeof_t0ype (s2e_t0ype: form): form

(* ****** ****** *)
//
fun
func_decl_0
  (name: string, res: sort): func_decl
fun
func_decl_1
  (name: string, arg: sort, res: sort): func_decl
fun
func_decl_2
  (name: string, a0: sort, a1: sort, res: sort): func_decl
//
fun
func_decl_list
  (name: string, domain: sortlst, range: sort): func_decl
//
fun
func_decl_to_smtlib
  (fd: func_decl): SMTAst

(* ****** ****** *)
//
fun
formula_fdapp_0(fd: func_decl): form
fun
formula_fdapp_1(fd: func_decl, arg: form): form
fun
formula_fdapp_2(fd: func_decl, a0: form, a1: form): form
fun
formula_fdapp_list(fd: func_decl, args: formlst): form
//
(* ****** ****** *)
//
fun
formula_to_smtlib(f: form): SMTAst
//
fun
declare_s2cst(s2cst): void
//
(* ****** ****** *)

absvtype smtenv_vtype = ptr
vtypedef smtenv = smtenv_vtype

(* ****** ****** *)
//
fun
smtenv_create(): smtenv
fun
smtenv_destroy(env: smtenv): void
//
(* ****** ****** *)
//
fun
s2var_pop_payload(s2var): form
fun
s2var_top_payload(s2var): form
fun
s2var_push_payload(s2var, form): void
//
overload .pop_payload with s2var_pop_payload
overload .top_payload with s2var_top_payload
overload .push_payload with s2var_push_payload
//
(* ****** ****** *)
//
fun
smtenv_add_s2var
  (env: !smtenv, s2v: s2var): void
fun
smtenv_add_s2exp
  (env: !smtenv, s2e: s2exp): void
fun
smtenv_add_h3ypo
  (env: !smtenv, h3p: h3ypo): void
//
(* ****** ****** *)
//
fun
formula_error_s2cst(s2c0: s2cst): form
fun
formula_error_s2exp(s2e0: s2exp): form
//
overload formula_error with formula_error_s2cst
overload formula_error with formula_error_s2exp
//
(* ****** ****** *)
//
fun
formula_make_s2cst
  (env: !smtenv, s2c: s2cst): form
fun
formula_make_s2cst_fresh
  (env: !smtenv, s2c: s2cst): form
//
fun
formula_make_s2var
  (env: !smtenv, s2v: s2var): form
fun
formula_make_s2var_fresh
  (env: !smtenv, s2v: s2var): form
//
fun
formula_make_s2Var_fresh
  (env: !smtenv, s2V: s2Var, s2t: s2rt): form
//
(* ****** ****** *)
//
fun
formula_make_s2exp
  (env: !smtenv, s2e: s2exp): form
fun
formulas_make_s2explst
  (env: !smtenv, s2es: s2explst): formlst
fun
formulas_make_labs2explst
  (env: !smtenv, ls2es: labs2explst): formlst
//
fun
formula_make_s2cst_s2explst
  (env: !smtenv, s2c: s2cst, s2es: s2explst): form
//
(* ****** ****** *)
//
datatype
s2cinterp =
//
  | S2CINTnone of ()
  | S2CINTsome of (ptr)
//
  | S2CINTbuiltin_0 of (() -> form)
  | S2CINTbuiltin_1 of (form -> form)
  | S2CINTbuiltin_2 of ((form, form) -> form)
//
  | S2CINTbuiltin_list of ((formlst) -<cloref1> form)
//
//
(* ****** ****** *)
//
fun print_s2cinterp (s2cinterp): void
and prerr_s2cinterp (s2cinterp): void
fun fprint_s2cinterp : fprint_type(s2cinterp)
//
overload print with print_s2cinterp
overload prerr with prerr_s2cinterp
overload fprint with fprint_s2cinterp
//
(* ****** ****** *)
//
fun
s2cst_get_s2cinterp(s2cst): s2cinterp
//
fun
s2cfun_initize_s2cinterp(s2c: s2cst): void
//
(* ****** ****** *)

absview smtenv_push_v

(* ****** ****** *)
//
fun smtenv_pop (smtenv_push_v | !smtenv): void
//
fun smtenv_push (env: !smtenv): (smtenv_push_v | void)
//
(* ****** ****** *)

fun the_s2cinterp_initize(): void

(* ****** ****** *)
//
fun
smtenv_formula_solve(!smtenv, form): lbool
// 
(* ****** ****** *)

(* end of [patsolve_smt_solving.sats] *)

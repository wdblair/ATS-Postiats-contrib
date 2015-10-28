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

#define :: list_vt_cons
#define nil list_vt_nil

(* ****** ****** *)
//
implement
s2var_pop_payload
  (s2v0) = ast where
{
//
val asts =
  s2var_get_payload(s2v0)
val asts =
  $UN.castvwtp0{List1_vt(form)}(asts)
//
val+~list_vt_cons(ast, asts) = asts
//
val ((*void*)) =
  s2var_set_payload(s2v0, $UN.castvwtp0{ptr}(asts))
//
} (* end of [s2var_pop_payload] *)
//
(* ****** ****** *)

implement
s2var_top_payload
  (s2v0) = let
//
val asts =
  s2var_get_payload(s2v0)
val asts =
  $UN.castvwtp0{List1_vt(form)}(asts)
//
val+list_vt_cons(ast, _) = asts
//
val ast2 = ast.incref()
//
prval ((*void*)) = $UN.cast2void(asts)
//
in
  $UN.castvwtp0{form}(ast2)
end // end of [s2var_top_payload]

(* ****** ****** *)

implement
s2var_push_payload
  (s2v0, ast) = let
//
val asts =
  s2var_get_payload(s2v0)
val asts =
  list_vt_cons(ast, $UN.castvwtp0{formlst}(asts))
//
in
  s2var_set_payload(s2v0, $UN.castvwtp0{ptr}(asts))
end (* end of [s2var_push_payload] *)

(* ****** ****** *)

assume smtenv_vtype = ptr
assume smtenv_push_v = unit_v

(* ****** ****** *)

implement
smtenv_create
  () = the_null_ptr where
{
(** Nothing to do for now *)
} (* end of [smtenv_create] *)

(* ****** ****** *)

implement
smtenv_destroy
  (env) = let
//
in
  // nothing
end // end of [smtenv_destroy]

(* ****** ****** *)

implement
smtenv_pop
  (pf | env) = let
//
prval unit_v() = pf
val () = println!("(pop)")
//
in
  // nothing
end // end of [smtenv_pop]

(* ****** ****** *)

implement
smtenv_push
  (env) = let
//
val () = println!("(push)")
//
in
  (unit_v() | ())
end // end of [smtenv_push]

(* ****** ****** *)

implement
smtenv_add_s2var
  (env, s2v0) = let
val ast =
  formula_make_s2var_fresh(env, s2v0)
//
in
  s2var_push_payload(s2v0, ast)
end // end of [smtenv_add_s2var]

(* ****** ****** *)

implement
smtenv_add_s2exp
  (env, s2p0) = let
//
val s2p0 =
  formula_make_s2exp(env, s2p0)
//
val ast = formula_to_smtlib(s2p0)
val assertion = Apply(copy("assert"), ast :: nil)
val decl = assertion.to_string()
val () = println! decl
val () = free(decl)
//
in
  // nothing
end // end of [smtenv_add_s2exp]

(* ****** ****** *)

implement
smtenv_add_h3ypo
  (env, h3p0) = let
//
(*
val () =
fprintln!
(
  stdout_ref
, "smtenv_add_h3ypo: h3p0 = ", h3p0
) (* end of [val] *)
*)
//
in
//
case+
h3p0.h3ypo_node of
| H3YPOprop s2p =>
    smtenv_add_s2exp(env, s2p)
  // end of [H3YPOprop]
//
| H3YPObind
    (s2v1, s2e2) => let
  in
    if s2var_is_impred(s2v1)
      then ()
      else let
        val s2p =
        s2exp_eqeq
          (s2exp_var(s2v1), s2e2)
        // end of [val]
      in
        smtenv_add_s2exp(env, s2p)
      end // end of [else]
  end // end of [H3YPObind]
//
| H3YPOeqeq
    (s2e1, s2e2) =>
  (
    smtenv_add_s2exp(env, s2exp_eqeq(s2e1, s2e2))
  ) (* end of [H3YPOeqeq] *)
//
end // end of [smtenv_add_h3ypo]

(* ****** ****** *)

implement
smtenv_formula_solve
  (env, s2p0) = let
//
val () =
  println! ("(push)")
//
val s2p1 = formula_not(s2p0)
val ast = formula_to_smtlib(s2p1)
val cmd = smtlib_to_string(Apply(copy("assert"), ast :: nil))
val () = println! (cmd)
val () = free (cmd)
//
val () = println! ("(check-sat)")
//
val () =
  println! ("(pop)")
//
in
  (** We're not interacting with the external solver yet, 
      assume all formulas are valid *)
  lfalse
//
end (* end of [smtenv_formula_solve] *)
(* ****** ****** *)

(* end of [patsolve_z3_solving_smtenv.dats] *)

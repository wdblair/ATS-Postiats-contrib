(*
##
## ATS-extsolve-smt:
## Solving ATS-constraints with arbitrary solvers
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

absvt@ype refcount(a:t@ype, n:int) = @{
	data=a
	references=int n
}

vtypedef Refcount(a:t@ype) = [n:int] refcount(a, n)

datavtype Ast(a:t@ype) =
  | Atom of (a)
  | Apply of (a, List_vt(a))
  
assume form_vtype = refcount(Ast(Strptr1),)
assume func_decl_vtype = 

(* ****** ****** *)

implement
formula_decref
  (ast) =
    
(* ****** ****** *)

implement
formula_incref
  (ast) = ast2 where
{
  val ast2 = strptr1_copy(ast)
}

(* ****** ****** *)

implement
formula_null
  ((*void*)) = formula_int(0)

(* ****** ****** *)

implement
formula_false() = tt where
{
  val tt = string1_copy("false")
}

(* ****** ****** *)

implement
formula_int(i) = i2 where
{
  val i2 = g0int2string(i)
}

implement
formula_intrep(i) = i2 where
{
  val i2 = string1_copy(i)
}

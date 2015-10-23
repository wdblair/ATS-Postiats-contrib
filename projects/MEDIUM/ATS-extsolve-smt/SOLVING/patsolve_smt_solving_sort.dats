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

assume sort_vtype = SMTAst

(* ****** ****** *)

implement
sort_to_smtlib
  (srt) = srt

(* ****** ****** *)

implement
sort_decref
  (ty) =
    case+ ty of
      | ~Atom (a) => strptr_free(a)
      | ~Apply (opr, args) => let
        val () = strptr_free(opr)
        implement
        list_vt_freelin$clear<SMTAst>(x) = $effmask_all(
          sort_decref(x)
        )
      in
        list_vt_freelin(args)
      end
      
implement
sort_incref
  (ty) = 
    case+ ty of
      | Atom (a) => ast2 where {
        val a2 = copy(a)
        val ast2 = Atom(a2)
      }
      | Apply(opr, args) => ast2 where {
        val opr2 = copy(opr)
        
        implement 
        list_vt_map$fopr<SMTAst><SMTAst>(x) =
          sort_incref(x)
        val args2 = list_vt_map<SMTAst><SMTAst>(args)
        val ast2 = Apply(opr2, args2)
      }

(* ****** ****** *)

implement 
sort_int () = res where
{
  val res = Atom(copy("Int"))
}

implement
sort_bool () = res where
{
  val res = Atom(copy("Bool"))
}

implement
sort_real () = res where
{
  val res = Atom(copy("Real"))
}

(* ****** ****** *)

implement
sort_mk_cls () = sort_mk_abstract("cls")
implement
sort_mk_eff () = sort_mk_abstract("eff")

(* ****** ****** *)
//
implement
sort_mk_type () = sort_mk_abstract("type")
implement
sort_mk_vtype () = sort_mk_abstract("type")
//
implement
sort_mk_t0ype () = sort_mk_abstract("t0ype")
implement
sort_mk_vt0ype () = sort_mk_abstract("t0ype")
//
implement
sort_mk_prop () = sort_bool()
implement
sort_mk_view () = sort_bool()
//
implement
sort_mk_tkind () = sort_mk_abstract ("tkind")

(* ****** ****** *)

implement
sort_declare_abstract
  (name) = println! ("(declare-sort ", name, " 1)")

(*
implement
sort_mk_abstract(name) = sort_int()
*)

implement
sort_mk_abstract
  (name) = res where
{
  val res = Atom(copy(name))
}

(* ****** ****** *)

implement
sort_error
  (s2t0) = res where
{
//
val () =
prerrln!
  ("sort_error: s2t0 = ", s2t0)
//
val () = assertloc(false)
val res = sort_error(s2t0)
//
} (* end of [sort_error] *)

(* ****** ****** *)

implement
sort_make_s2rt(s2t0) = let
//
(*
val () =
println! ("sort_make: s2t0 = ", s2t0)
*)
//
in
//
case+ s2t0 of
//
| S2RTint() => sort_int()
| S2RTaddr() => sort_int()
| S2RTbool() => sort_bool()
//
| S2RTreal() => sort_real()
(*
| S2RTstring() => sort_string()
*)
//
| S2RTcls() => sort_mk_cls()
| S2RTeff() => sort_mk_eff()
//
| S2RTtype() => sort_mk_type()
| S2RTvtype() => sort_mk_vtype()
| S2RTt0ype() => sort_mk_t0ype()
| S2RTvt0ype() => sort_mk_vt0ype()
//
| S2RTprop() => sort_mk_prop()
| S2RTview() => sort_mk_view()
//
| S2RTtkind() => sort_mk_tkind()
//
| S2RTnamed(sym) =>
    sort_mk_abstract(sym.name())
  // end of [S2RTnamed]
//
| _(*rest-of-S2RT*) => sort_error(s2t0)
//
end (* end of [sort_make_s2rt] *)

(* ****** ****** *)

(* end of [patsolve_z3_solving_sort.dats] *)
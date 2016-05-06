
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



implement sort_bag () = Atom(copy("Bag"))
implement sort_bag_elt () = Atom(copy("BagElt"))
implement sort_set () = Atom(copy("Set"))
implement sort_set_elt () = Atom(copy("SetElt"))
implement sort_list () = Atom(copy("MyList"))
implement sort_list_elt () = Atom(copy("ListElt"))



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
  (** 
    I think there should be separate sorts for
    reals and floats because often the user will
    specify constants using real numbers, whereas
    converting them to floats in the statics will 
    allow us to capture their actual meaning in the
    program.
  *)
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
  (name) = println! ("(declare-sort ", name, " 0)")

implement
sort_declare_alias
  (alias, sort) = println! ("(define-sort ", alias, 
                            " ()", 
                            " (",sort, "))")
                            
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
    (
    case+ sym.name() of
//    | "Bag" => sort_bag ()
//    | "BagElt" => sort_bag_elt ()
//    | "Set" => sort_set ()
//    | "SetElt" => sort_set_elt ()
//    | "List" => sort_list ()
//    | "ListElt" => sort_list_elt ()
    | "real" => sort_real()
    | _ => sort_mk_abstract(sym.name())
    )
  // end of [S2RTnamed]
//
| _(*rest-of-S2RT*) => sort_error(s2t0)
//
end (* end of [sort_make_s2rt] *)

(* ****** ****** *)

#define :: list_vt_cons
#define nil list_vt_nil

local
  val i = ref_make_elt<int>(0)
in

implement
sort_declare_s2rtdat (s2rtdat) = let
  val name = s2rtdat_get_name (s2rtdat)
  val conlst = s2rtdat_get_sconlst (s2rtdat)
  val constructors =
    list_map_fun<s2cst><SMTAst>(conlst,
      lam s2cst => let
        val stamp = s2cst.stamp()
        val sym = s2cst.name()
        val name = sym.name()
        val id = stringlst_concat(
          list_cons(name, list_cons("!", list_cons(strptr2string(g0int2string(stamp_get_int(stamp))), list_nil())))
        )
        val- S2RTfun(args, res) = s2cst.srt()
        val n = length(args)
        fun range (n:int, res:List0(int)): List0(int) =
          if n = 0 then
            res
          else let
            val nexti = !i
            val () = !i := succ(nexti)
          in
           range(pred(n), list_cons(nexti, res))
          end
          val iargs = list_zip(range(n, list_nil()), args)
          val declargs = list_vt_map_fun<@(int, s2rt)><SMTAst>(iargs,
            lam pair => let
              val i = pair.0
              val srt = pair.1
              val ast = sort_make_s2rt(srt)
              val id = g0int2string(i)
              val name = strptrlst_concat (copy("x") :: id :: nil)
              val () = assertloc(isneqz(name))
            in
              Apply(name, ast :: nil)
            end 
          )
          val () = list_vt_free(iargs)
      in        
        Apply(id, declargs)
      end
    ) // end of [constructors]
in
  case+ constructors of
    | ~list_vt_nil () => 
      () (** Don't declare it, an alias should be made in the constraint solver. *)
    | _ =>> let 
      val cs = 
        list_vt_mapfree_fun<SMTAst><Strptr1>(constructors, lam c => c.to_string())
      val consdecl = strptrlst_concat(cs)
    in
      println!("(declare-datatypes () ((", name, " ", consdecl, ")))");
      free(consdecl)
    end
end // end of [sort_declare_s2rtdat]

end // end of [local]

(* ****** ****** *)

(* end of [patsolve_z3_solving_sort.dats] *)
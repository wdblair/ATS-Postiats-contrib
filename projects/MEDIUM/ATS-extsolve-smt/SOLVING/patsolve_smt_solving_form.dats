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

#define :: cons_vt
#define nil nil_vt

(* ****** ****** *)

vtypedef func_decl_record = @{
  symbol=string,
  args=sortlst,
  res=sort
}

assume form_vtype = SMTAst
assume func_decl_vtype = ref(func_decl_record)

(* ****** ****** *)

implement
formula_to_smtlib 
  (f) = f

(* ****** ****** *)

fun formlst_to_smtlib
  (fs: formlst): SMTAst = let
    val opr = copy("")
in
  Apply(opr, fs)
end

(* ****** ****** *)

implement
formula_decref
  (ast) =
    case+ ast of
      | ~Atom (a) => strptr_free(a)
      | ~Apply (opr, args) => let
        val () = free(opr)
        implement
        list_vt_freelin$clear<SMTAst>(x) = $effmask_all(
          formula_decref(x)
        )
      in
        list_vt_freelin(args)
      end
      
(* ****** ****** *)

implement
formula_incref
  (ast) =
    case+ ast of
      | Atom (a) => ast2 where {
        val a2 = strptr1_copy(a)
        val ast2 = Atom(a2)
      }
      | Apply(opr, args) => ast2 where {
        val opr2 = strptr1_copy(opr)
        
        implement 
        list_vt_map$fopr<SMTAst><SMTAst>(x) =
          formula_incref(x)
        val args2 = list_vt_map<SMTAst><SMTAst>(args)
        val ast2 = Apply(opr2, args2)
      }
      
(* ****** ****** *)

implement
formula_null
  ((*void*)) = formula_int(0)
  
(* ****** ****** *)

implement
formula_true() = tt where
{
  val f = string0_copy("true")
  val tt = Atom(f)
}

implement
formula_false() = tt where
{
  val f = string0_copy("false")
  val tt = Atom(f)
}

(* ****** ****** *)

implement
formula_int(i) = i2 where
{
  val i2 = Atom(g0int2string(i))
}

implement
formula_intrep(i) = i2 where
{
  val i2 = Atom(string0_copy(i))
}

implement
formula_not
  (f) = res where
{
  val opr = string0_copy("not")
  val res = Apply(opr, f :: nil)
} (* end of [formula_not] *)

implement
formula_disj
  (p, q) = res where
{
  val opr = string0_copy("or")
  val res = Apply(opr, p :: q :: nil)
} (* end of [formula_disj] *)

implement
formula_conj
  (p, q) = res where
{
  val opr = string0_copy("and")
  val res = Apply(opr, p :: q :: nil)
} (* end of [formula_conj] *)

implement
formula_impl
  (p, q) = res where
{
  val opr = string0_copy("=>")
  val res = Apply(opr, p :: q :: nil)
} (* end of [formula_impl] *)

implement
formula_conj_list
  (ps) = res where 
{
  val opr = string0_copy("and")
  val res = Apply(opr, ps)
}

implement
formula_conj_list1
  (args, res) = 
    case+ args of
      | ~list_vt_nil() => res
      | _ => let
        val conjargs = formula_conj_list(args)
      in    
        formula_conj(conjargs, res)
      end
      
implement
formula_impl_list1
  (args, res) =
  case+ args of
    | ~list_vt_nil() => res
    | _ => let
      val conjargs = formula_conj_list(args)
    in
      formula_impl(conjargs, res)
    end

(* ****** ****** *)    

implement
formula_ineg
  (n) = res where
{
  val opr = string0_copy("-")
  val res = Apply(opr, n::nil)
}

implement
formula_iadd
  (x, y) = res where
{
  val opr = string0_copy("+")
  val res = Apply(opr, x::y::nil)
}

implement
formula_isub
  (x, y) = res where
{
  val opr = string0_copy("-")
  val res = Apply(opr, x::y::nil)
}

implement
formula_imul
  (x, y) = res where
{
  val opr = string0_copy("*")
  val res = Apply(opr, x::y::nil)
}

implement
formula_idiv
  (x, y) = res where
{
  val opr = string0_copy("div")
  val res = Apply(opr, x::y::nil)
}

implement
formula_ndiv
  (x, y) = res where
{
  val opr = string0_copy("div")
  val res = Apply(opr, x::y::nil)
}

implement
formula_ilt
  (x, y) = res where
{
  val opr = string0_copy("<")
  val res = Apply(opr, x::y::nil)
}

implement
formula_ilte
  (x, y) = res where
{
  val opr = string0_copy("<=")
  val res = Apply(opr, x::y::nil)
}

implement
formula_igt
  (x, y) = res where
{
  val opr = string0_copy(">")
  val res = Apply(opr, x::y::nil)
}

implement
formula_igte
  (x, y) = res where
{
  val opr = string0_copy(">=")
  val res = Apply(opr, x::y::nil)
}

implement
formula_ieq
  (x, y) = res where
{
  val opr = string0_copy("=")
  val res = Apply(opr, x::y::nil)
}

implement
formula_ineq
  (x, y) =
  formula_not(formula_ieq(x, y))
  
implement
formula_iabs
  (x) = res where
{
  val  opr = string0_copy("abs")
  val res = Apply(opr, x::nil)
}

implement
formula_isgn
  (x) = let
    val x' = formula_incref(x)
    val gtz = formula_igt(x, formula_int(0))
    val ltz = formula_igt(x', formula_int(0))
in
  formula_cond(gtz, formula_int(1), formula_cond(ltz, formula_int(~1), formula_int(0)))
end

implement
formula_imax
  (x, y) = let
    val x' = formula_incref(x)
    val y' = formula_incref(y)
  in
    formula_cond(formula_igte(x, y), x', y')
  end
  
implement
formula_imin
  (x, y) = let
    val x' = formula_incref(x)
    val y' = formula_incref(y)
  in
    formula_cond(formula_ilte(x, y), x', y')
  end  

(* ****** ****** *)

implement
formula_bneg(s2e) = formula_not(s2e)

implement
formula_badd
  (p, q) = formula_disj(p, q)
  
implement
formula_bmul
  (p, q) = formula_conj(p, q)  

(* ****** ****** *)

implement
formula_blt
  (p, q) =
  formula_conj(formula_not(p), q)

implement
formula_blte
  (p, q) = formula_impl(p, q)

implement
formula_bgt
  (p, q) =
  formula_conj(p, formula_not(q))
  
implement
formula_bgte
  (p, q) = formula_impl(q, p)
  
(* ****** ****** *)

implement
formula_beq
  (p, q) = res where 
{
  val opr = string0_copy("=")
  val res = Apply(opr, p :: q :: nil)
}

implement
formula_bneq
  (p, q) =
  formula_not(formula_beq(p, q))

(* ****** ****** *)

implement
formula_cond
  (test, t, f) = res where
{
  val opr = string0_copy("if")
  val res = Apply(opr, test :: t :: f :: nil)
}

implement
formula_eqeq
  (p, q) = res where
{
  val opr = string0_copy("=")
  val res = Apply(opr, p :: q :: nil)
}

implement
formula_sizeof_t0ype
  (s2e) = let
//
val r = sort_int()
val a = sort_mk_t0ype()
//
val fd =
  func_decl_1("sizeof_t0ype", a, r)
//
in
  formula_fdapp_1(fd, s2e)
end // end of [formula_sizeof]

(* ****** ****** *)

implement
func_decl_0
  (name, res) = let
  var decl : func_decl_record
  val () = begin
    decl.symbol := name;
    decl.args := nil;
    decl.res := res
  end
in
  ref_make_elt<func_decl_record>(decl)
end

implement
func_decl_1
  (name, arg, res) = let
  var decl : func_decl_record
  val () = begin
    decl.symbol := name;
    decl.args := arg :: nil;
    decl.res := res
  end
in
  ref_make_elt<func_decl_record>(decl)
end

implement
func_decl_2
  (name, a0, a1, res) = let
  var decl : func_decl_record
  val () = begin
    decl.symbol := name;
    decl.args := a0 :: a1 :: nil;
    decl.res := res
  end
in
  ref_make_elt<func_decl_record>(decl)
end

(* ****** ****** *)

implement
formula_fdapp_1
  (fd, arg) = res where
{
//
val opr = copy(fd->symbol)
val res = Apply(opr, arg :: nil)
//
} // end of [formula_fdapp_1]

implement
formula_fdapp_list
  (f, args) = res where
{
  val name = f->symbol
  val res = Apply(copy(name), args)
} // end of [formula_fdapp_list]

implement
func_decl_list
  (name, args, res) = let
  var decl : func_decl_record
  val () = begin
    decl.symbol := name;
    decl.args := args;
    decl.res := res
  end
  in
    ref_make_elt<func_decl_record>(decl)
  end // end of [func_decl_list]
  
implement
func_decl_to_smtlib
  (fd) = let
  val (pf, fpf | fp) = ref_vtakeout{func_decl_record}(fd)
  val opr = copy("declare-fun")
  val name = copy(fp->symbol)
  val sorts =
    list_vt_map_fun<sort><form>(fp->args,
                                lam s => sort_to_smtlib(sort_incref(s)))
  val res = sort_incref(fp->res)
  val resast = sort_to_smtlib(res)
  prval () = fpf(pf)
in
  Apply(opr, Atom(name) :: formlst_to_smtlib(sorts) :: resast :: nil)
end

implement
declare_s2cst
  (s2cst) = let
  val stamp = stamp_get_int(s2cst.stamp())
  val srt = s2cst.srt()
  val sym = s2cst.name()
  val name = sym.name()
  val id = strptrlst_concat(copy(name) :: copy("!") :: g0int2string(stamp) :: nil)
  val () = assertloc(isneqz(id))
in
  case+ srt of
    | S2RTfun (args, res) => let
      val domain = list_map_fun<s2rt><sort>(args, lam x => sort_make_s2rt(x))
      val range = sort_make_s2rt(res)
      val idcopy = strptr2string(copy(id))
      val decl = func_decl_list(idcopy, domain, range)
      val ast = func_decl_to_smtlib(decl)
      val smtcmd = ast.to_string()
    in
      println! smtcmd;
      free(smtcmd);
      free(id)
    end
    | _ =>> let
      val opr = copy("declare-const")
      val sort = sort_to_smtlib(sort_make_s2rt(srt))
      val decl = Apply(opr, Atom(copy(id)) :: sort :: nil)
      val cmd = decl.to_string()
    in
      println! cmd;
      free(cmd);
      free(id)
    end
end // end of [delcare_s2cst]

(* ****** ****** *)

implement
formula_error_s2cst
  (s2c0) = res where
{
//
val () =
prerrln!
  ("formula_error: s2c0 = ", s2c0)
//
val () = assertloc(false)
val res = formula_error_s2cst(s2c0)
//
} (* end of [formula_error_s2cst] *)

implement
formula_error_s2exp
  (s2e0) = res where
{
//
val () =
prerrln!
  ("formula_error: s2e0 = ", s2e0)
//
val () = assertloc(false)
val res = formula_error_s2exp(s2e0)
//
} (* end of [formula_error_s2exp] *)

(* ****** ****** *)

implement
formula_make_s2cst
  (env, s2c0) = let
//
val s2ci = s2cst_get_s2cinterp(s2c0)
//
in
//
case+ s2ci of
//
| S2CINTnone() => let
    val s2e = 
    formula_make_s2cst_fresh(env, s2c0)
    val s2e_ = formula_incref(s2e)
    val s2e =
    S2CINTsome($UN.castvwtp0{ptr}(s2e))
    val ((*void*)) =
      s2cst_set_payload(s2c0, $UN.cast{ptr}(s2e))
    // end of [val]
  in
    s2e_
  end // end of [S2CINTnone]
//
| S2CINTsome(ptr) => let
    val s2e =
      $UN.castvwtp0{form}(ptr)
    val s2e_ = formula_incref(s2e)
    prval () = $UN.cast2void(s2e)
  in
    s2e_
  end // end of [S2CINTsome]
//
| S2CINTbuiltin_0(f) => f((*void*))
//
| _(*rest-of-S2CINT*) => formula_error(s2c0)
//
end // end of [formula_make_s2cst]

(* ****** ****** *)

implement
formula_make_s2cst_fresh
  (env, s2c0) = ast where
{
//
val sym = s2c0.name()
val name = copy(sym.name())
val stamp = stamp_get_int(s2c0.stamp())
val id = strptrlst_concat(name :: copy("!") :: g0int2string(stamp) :: nil)
val () = assertloc(isneqz(id))
val ast = Atom(id)
//
} (* end of [formula_make_s2cst_fresh] *)

(* ****** ****** *)

implement
formula_make_s2var
  (env, s2v0) = let
//
(*
val () =
println!
(
 "formula_make_s2var: s2v0 = ", s2v0
) (* end of [val] *)
val () =
println!
(
 "formula_make_s2var: s2v0.stamp = ", s2v0.stamp()
) (* end of [val] *)
*)
//
val
ptr =
s2var_get_payload(s2v0)
//
in
//
if
ptr > 0
then s2var_top_payload(s2v0)
else formula_make_s2var_fresh(env, s2v0)
//
end // end of [formula_make_s2var]

(* ****** ****** *)

implement
formula_make_s2var_fresh
  (env, s2v0) = ast where
{
//
val sym = s2v0.name()
val name = copy(sym.name())
val stamp = stamp_get_int(s2v0.stamp())
val id = strptrlst_concat(name :: copy("!") :: g0int2string(stamp) :: nil)
//
val () = assertloc(isneqz id)
val ast = Atom(id)
val ast2 = ast.incref()
//
val srt = sort_make_s2rt(s2v0.srt())
val srtast = sort_to_smtlib(srt)
val decl = Apply(copy("declare-const"), formula_to_smtlib(ast2) :: srtast :: nil)
val cmd = decl.to_string()
val () = println!(cmd)
val () = free(cmd)
//
} (* end of [formula_make_s2var_fresh] *)

(* ****** ****** *)

implement
formula_make_s2Var_fresh
  (env, s2V0, s2t0) = ast where
{
//
val stamp =
  stamp_get_int(s2V0.stamp())
//
val stamp = g0int2string(stamp)
//
val
id =
string0_append
  ("s2Var$", $UN.strptr2string(stamp))
//
val () = strptr_free(stamp)
val ast = Atom(id)
} (* end of [formula_make_s2Var_fresh] *)

(* ****** ****** *)

implement
s2cfun_initize_s2cinterp
  (s2c0) = let
//
val stamp = s2c0.stamp()
val name = s2c0.name()
val name = symbol_get_name(name)
//
val s2t0 = s2c0.srt()
val-S2RTfun(s2ts_arg, s2t_res) = s2t0
(*
val arity = list_length(s2ts_arg)
*)
val range = sort_make_s2rt(s2t_res)
//
val domain =
  list_map_fun<s2rt><sort>(s2ts_arg, sort_make_s2rt)
// end of [val]
//
val id = strptr2string(stringlst_concat(list_cons(name, list_cons("!", list_cons(strptr2string(g0int2string(stamp_get_int(stamp))), list_nil())))))
val fd0 = func_decl_list(id, domain, range)
//
(** Ensure this function is not a datasort constructor.
    All datasort constructors are already defined *)
val s2rtdatmap = the_s2rtdatmap_get()
//
fun find(rs: s2rtdatlst, stamp: stamp): bool =
    (** datasort constructors are already declared*)
    case+ rs of
      | list_nil () => false
      | list_cons (dat, rss) => let
        val conss = s2rtdat_get_sconlst (dat)
        fun loop (cs: s2cstlst): bool =
          case+ cs of 
            | list_nil () => false
            | list_cons (s2cst, css) => let
              val stamp' = s2cst.stamp()
            in
              if stamp = stamp' then
                true
              else
                loop(css)
            end
       in
        loop(conss) orelse find (rss, stamp)
       end
//
val iscons = find(s2rtdatmap, stamp)
(**
val () =
  (** Declare the function to the smt solver *)
  if ~iscons then {
    val decl = func_decl_to_smtlib(fd0)
    val cmd = decl.to_string()
    val () = println!(cmd)
    val () = free(cmd)
  }
*)
//
val
fopr = lam
(
  xs: formlst
) : form =<cloref1> let
in
  (** Nullary constructors are not functions in smt-lib,
     they are constants in smt-which really screws things up. *)
  if iscons andalso length(xs) = 0 then let
    (** Typechecking won't pass without this check... *)
    val () = assertloc(length(xs) = 0)
    val ~list_vt_nil() = xs
  in
    Atom(copy(id))
  end
  else
    formula_fdapp_list(fd0, xs)
end // end of [fopr]
//
in
  s2cst_set_payload(s2c0, $UN.cast{ptr}(S2CINTbuiltin_list(fopr)))
end // end of [s2cfun_initize_s2cinterp]

(* ****** ****** *)

local

fun
aux_S2Ecst
(
  env: !smtenv, s2e0: s2exp
) : form = let
//
val-S2Ecst(s2c) = s2e0.s2exp_node
//
in
  formula_make_s2cst(env, s2c)
end // end of [aux_S2Ecst]

(* ****** ****** *)

fun
aux_S2Evar
(
  env: !smtenv, s2e0: s2exp
) : form = let
//
val-S2Evar(s2v) = s2e0.s2exp_node
//
in
  formula_make_s2var(env, s2v)
end // end of [aux_S2Evar]

(* ****** ****** *)

fun
aux_S2EVar
(
  env: !smtenv, s2e0: s2exp
) : form = let
//
val s2t = s2e0.s2exp_srt
val-S2EVar(s2V) = s2e0.s2exp_node
//
in
  formula_make_s2Var_fresh(env, s2V, s2t)
end // end of [aux_S2EVar]

(* ****** ****** *)

fun
aux_S2Eeqeq
(
  env: !smtenv, s2e0: s2exp
) : form = let
//
val-
S2Eeqeq(s2e1, s2e2) = s2e0.s2exp_node
//
val s2e1 = formula_make_s2exp(env, s2e1)
and s2e2 = formula_make_s2exp(env, s2e2)
in
  formula_eqeq (s2e1, s2e2)
end // end of [aux_S2Eeqeq]

(* ****** ****** *)

fun
aux_S2Eapp
(
  env: !smtenv, s2e0: s2exp
) : form = let
//
val-S2Eapp
  (s2e_fun, s2es_arg) = s2e0.s2exp_node
//
in
//
case+
s2e_fun.s2exp_node
of // case+
| S2Ecst(s2c) => let
    val s2ci =
      s2cst_get_s2cinterp(s2c)
    // end of [val]
  in
    case+ s2ci of
    | S2CINTbuiltin_0(f) => f()
    | S2CINTbuiltin_1(f) => let
        val-
        list_cons
          (s2e1, s2es_arg) = s2es_arg
        // end of [val]
        val s2e1 = formula_make_s2exp(env, s2e1)
      in
        f(s2e1)
      end // end of [S2CINTbuiltin_1]
    | S2CINTbuiltin_2(f) => let
        val-
        list_cons
          (s2e1, s2es_arg) = s2es_arg
        // end of [val]
        val-
        list_cons
          (s2e2, s2es_arg) = s2es_arg
        // end of [val]
        val s2e1 = formula_make_s2exp(env, s2e1)
        val s2e2 = formula_make_s2exp(env, s2e2)
      in
        f(s2e1, s2e2)
      end // end of [S2CINTbuiltin_2]
//
    | S2CINTbuiltin_list(f) =>
        f(formulas_make_s2explst(env, s2es_arg))
      // end of [S2CINTbuiltin_list]
//
    | S2CINTsome _ => formula_error(s2e0)
//
    | S2CINTnone() =>
        aux_S2Eapp(env, s2e0) where
      {
        val ((*void*)) = s2cfun_initize_s2cinterp(s2c)
      } (* [S2CINTnone] *)
//
  end // end of [S2Ecst]
| _(*non-S2Ecst*) => formula_error(s2e0)
//
end // end of [aux_S2Eapp]

(* ****** ****** *)

fun
aux_S2Emetdec
(
  env: !smtenv, s2e0: s2exp
) : form = let
//
val-
S2Emetdec
  (s2es_met, s2es_bnd) = s2e0.s2exp_node
//
(*
val () =
println!
  ("aux_S2Emetdec: s2es_met = ", s2es_met)
//
val () =
println!
  ("aux_S2Emetdec: s2es_bnd = ", s2es_bnd)
*)
//
fun
auxlst
(
  env: !smtenv
, s2es10: s2explst
, s2es20: s2explst
) : form =
(
case+ s2es10 of
| list_nil
    ((*void*)) => formula_false()
| list_cons
    (s2e1, s2es1) => let
    val-
    list_cons
      (s2e2, s2es2) = s2es20
    // end of [val]
    val s2e1 =
      formula_make_s2exp (env, s2e1)
    val s2e2 =
      formula_make_s2exp (env, s2e2)
  in
    case+ s2es1 of
    | list_nil _ => formula_ilt(s2e1, s2e2)
    | list_cons _ => let
        val s2e1_ = formula_incref (s2e1)
        val s2e2_ = formula_incref (s2e2)
        val s2e_ilt = formula_ilt(s2e1, s2e2) 
        val s2e_ilte = formula_ilte(s2e1_, s2e2_)
      in
        formula_disj(s2e_ilt, formula_conj(s2e_ilte, auxlst(env, s2es1, s2es2)))
      end // end of [list_cons]
  end // end of [list_cons]
)
//
in
  auxlst(env, s2es_met, s2es_bnd)
end // end of [aux_S2Emetdec]

(* ****** ****** *)

fun
aux_S2Etop
(
  env: !smtenv, s2e0: s2exp
) : form = let
//
val-
S2Etop(_, s2e) = s2e0.s2exp_node
//
in
  formula_make_s2exp(env, s2e)
end // end of [aux_S2Etop]

(* ****** ****** *)

fun
aux_S2Einvar
(
  env: !smtenv, s2e0: s2exp
) : form = let
//
val-
S2Einvar(s2e) = s2e0.s2exp_node
//
in
  formula_make_s2exp(env, s2e)
end // end of [aux_S2Einvar]

(* ****** ****** *)

fun
aux_S2Esizeof
(
  env: !smtenv, s2e0: s2exp
) : form = let
//
val-
S2Esizeof(s2e) = s2e0.s2exp_node
//
val s2e = formula_make_s2exp(env, s2e)
//
in
  formula_sizeof_t0ype(s2e)
end // end of [aux_S2Esizeof]

(* ****** ****** *)

fun
aux_S2Efun
(
  env: !smtenv, s2e0: s2exp
) : form = let
//
val
s2t0 = s2e0.s2exp_srt
//
val-
S2Efun
(npf, s2es, s2e_res) = s2e0.s2exp_node
//
in
//
case+ s2t0 of
//
| S2RTprop() => let
    val s2es = formulas_make_s2explst(env, s2es)
    val s2e_res = formula_make_s2exp(env, s2e_res)
  in
    formula_impl_list1 (s2es, s2e_res)
  end // end of [S2Efun]
//
| _(*non-prop*) => formula_error_s2exp(s2e0)
//
end // end of [aux_S2Efun]

(* ****** ****** *)

fun
aux_S2Etyrec
(
  env: !smtenv, s2e0: s2exp
) : form = let
//
val
s2t0 = s2e0.s2exp_srt
//
val-
S2Etyrec
(knd, npf, ls2es) = s2e0.s2exp_node
//
in
//
case+ s2t0 of
//
| S2RTprop() => let
    val s2es =
      formulas_make_labs2explst(env, ls2es)
    // end of [val]
  in
    formula_conj_list(s2es)
  end // end of [S2RTprop]
//
| _(*non-prop*) => formula_error_s2exp(s2e0)
//
end // end of [aux_S2Etyrec]

(* ****** ****** *)

vtypedef
symlst = List0_vt(string)

(* ****** ****** *)

extern
fun
formula_quant
(
  knd: int
, s2ts: form
, s2ps: formlst, s2e_body: form
) : form // end-of-function

(* ****** ****** *)

implement
formula_quant
(
  knd, s2ts, s2ps, s2e_body
) = let
//
val opr = (if knd > 0 
               then "forall" 
               else "exists"): string
val opr = copy(opr)
val body = (if knd > 0
            then formula_impl_list1(s2ps, s2e_body)
            else formula_conj_list1(s2ps, s2e_body)): form
in
  Apply(opr, s2ts :: body :: nil)
end // end of [formula_quant]

(* ****** ****** *)

fun
aux_quant
(
  knd: int
, env: !smtenv
, s2vs: s2varlst, s2ps: s2explst, s2e_body: s2exp
) : form =
(
case+ s2vs of
//
| list_nil() => let
    val s2ps = formulas_make_s2explst (env, s2ps)
    val s2e_body = formula_make_s2exp (env, s2e_body)
  in
    if knd > 0
      then formula_impl_list1(s2ps, s2e_body)
      else formula_conj_list1(s2ps, s2e_body)
    // end of [if]
  end // end of [list_nil()]
//
| list_cons _ => aux_quant2(knd, env, s2vs, s2ps, s2e_body)
) (* end of [aux_quant] *)

and
aux_quant2
(
  knd: int
, env: !smtenv
, s2vs: s2varlst, s2ps: s2explst, s2e_body: s2exp
) : form = let
//
fun
auxlst1
(
  s2vs: s2varlst, forms: List0_vt(form)
) : form = (
//
case+ s2vs of
| list_nil
    ((*void*)) => Apply(copy(""), forms)
  // end of [list_nil]
| list_cons
    (s2v, s2vs) => let
    val sym = s2v.name()
    val name = sym.name()
    val id = copy(name)
    val id' = copy(name)
    val variable = Atom(id)
    val () = s2v.push_payload(variable)
    val sort = sort_make_s2rt(s2v.srt())
    val sortast = sort_to_smtlib(sort)
    val decl = Apply(id', sortast :: nil)
  in
    auxlst1(s2vs, decl :: forms)
  end // end of [list_vt_cons]
//
) (* end of [auxlst1] *)
//
val s2ts = auxlst1 (s2vs, nil)
val s2ps = formulas_make_s2explst (env, s2ps)
val s2e_body = formula_make_s2exp (env, s2e_body)
//
val ((*void*)) =
list_foreach_fun<s2var>
(
  s2vs
, lam s2v =<fun1> formula_decref(s2var_pop_payload(s2v))
) (* end of [val] *)
//
in
  formula_quant(knd, s2ts, s2ps, s2e_body)
end // end of [aux_quant2]

fun
aux_S2Euni
(
  env: !smtenv, s2e0: s2exp
) : form = let
//
val
s2t0 = s2e0.s2exp_srt
//
(*
val () =
println!
  ("aux_S2Euni: s2t0 = ", s2t0)
*)
//
val-
S2Euni
(s2vs, s2ps, s2e) = s2e0.s2exp_node
//
in
//
case+ s2t0 of
//
| S2RTprop() =>
    aux_quant(1(*uni*), env, s2vs, s2ps, s2e)
  // end of [S2RTprop]
//
| _(*non-prop*) => formula_error_s2exp(s2e0)
//
end // end of [aux_S2Euni]

fun
aux_S2Eexi
(
  env: !smtenv, s2e0: s2exp
) : form = let
//
val
s2t0 = s2e0.s2exp_srt
//
(*
val () =
println!
  ("aux_S2Eexi: s2t0 = ", s2t0)
*)
//
val-
S2Eexi
(s2vs, s2ps, s2e) = s2e0.s2exp_node
//
in
//
case+ s2t0 of
//
| S2RTprop() =>
    aux_quant(0(*knd*), env, s2vs, s2ps, s2e)
  // end of [S2RTprop]
//
| _(*non-prop*) => formula_error_s2exp(s2e0)
//
end // end of [aux_S2Eexi]

(* ****** ****** *)

in (* in-of-local *)

implement
formula_make_s2exp
  (env, s2e0) = let
//
(*
val () =
println!
  ("formula_make_s2exp: s2e0 = ", s2e0)
*)
//
in
//
case+
s2e0.s2exp_node
of // case+
//
| S2Eint(i) => formula_int(i)
| S2Eintinf(rep) => formula_intrep(rep)
//
| S2Ecst _ => aux_S2Ecst(env, s2e0)
| S2Evar _ => aux_S2Evar(env, s2e0)
//
| S2EVar _ => aux_S2EVar(env, s2e0)
//
| S2Eeqeq _ => aux_S2Eeqeq(env, s2e0)
//
| S2Eapp _ => aux_S2Eapp (env, s2e0)
//
| S2Emetdec _ => aux_S2Emetdec (env, s2e0)
//
| S2Etop _=> aux_S2Etop (env, s2e0)
//
| S2Einvar _ => aux_S2Einvar (env, s2e0)
//
| S2Esizeof _ => aux_S2Esizeof (env, s2e0)
//
| S2Efun _ => aux_S2Efun (env, s2e0)
//
| S2Euni _ => aux_S2Euni (env, s2e0)
| S2Eexi _ => aux_S2Eexi (env, s2e0)
//
| S2Etyrec _ => aux_S2Etyrec (env, s2e0)
//
| _ (*unrecognized*) => formula_error(s2e0)
//
end // end of [formula_make_s2exp]

end // end of [local]

(* ****** ****** *)

implement
formulas_make_s2explst
  (env, s2es) = (
//
case+ s2es of
| list_nil
    ((*void*)) => list_vt_nil()
| list_cons
    (s2e, s2es) => let
    val s2e = formula_make_s2exp(env, s2e)
    val s2es = formulas_make_s2explst(env, s2es)
  in
    list_vt_cons(s2e, s2es)
  end // end of [list_cons]
//
) (* end of [formulas_make_s2explst] *)

(* ****** ****** *)

implement
formulas_make_labs2explst
  (env, ls2es) = (
//
case+ ls2es of
| list_nil
    ((*void*)) => list_vt_nil()
| list_cons
    (ls2e, ls2es) => let
    val+SLABELED(l, s2e) = ls2e
    val s2e = formula_make_s2exp(env, s2e)
    val s2es = formulas_make_labs2explst(env, ls2es)
  in
    list_vt_cons(s2e, s2es)
  end // end of [list_cons]
//
) (* end of [formulas_make_s2explst] *)

(* ****** ****** *)
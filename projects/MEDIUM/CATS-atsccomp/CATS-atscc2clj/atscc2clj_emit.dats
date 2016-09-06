(* ****** ****** *)
//
// Atscc2clj:
// from ATS to Clojure
//
(* ****** ****** *)
//
// HX-2016-05-19: start
//
(* ****** ****** *)
//
#include
"share/atspre_staload.hats"
//
(* ****** ****** *)
//
staload
UN = "prelude/SATS/unsafe.sats"
//
(* ****** ****** *)
//
#define
CATSPARSEMIT_targetloc "./.CATS-parsemit"
//
(* ****** ****** *)
//
staload "{$CATSPARSEMIT}/catsparse.sats"
//
staload "{$CATSPARSEMIT}/catsparse_syntax.sats"
//
staload "{$CATSPARSEMIT}/catsparse_emit.sats"
//
staload "{$CATSPARSEMIT}/catsparse_typedef.sats"
staload "{$CATSPARSEMIT}/catsparse_fundecl.sats"
//
(* ****** ****** *)
//
extern
fun
emit_tmpvar_val
  (out: FILEref, tmp: i0de): void
//
(* ****** ****** *)

local
//
staload
TM =
"libats/libc/SATS/time.sats"
//
typedef time_t = $TM.time_t
//
in (* in-of-local *)

implement
emit_time_stamp (out) = let
//
var tm: time_t
val () = tm := $TM.time_get ()
val (pfopt | p_tm) = $TM.localtime (tm)
//
val () = emit_text (out, ";;;;;;\n");
val () = emit_text (out, ";;\n");
val () = emit_text (out, ";; The Clojure code is generated by atscc2clj\n")
val () = emit_text (out, ";; The starting compilation time is: ")
//
val () =
if
p_tm > 0
then let
//
  prval
  Some_v@(pf1, fpf1) = pfopt
//
  val tm_min = $TM.tm_get_min (!p_tm)
  val tm_hour = $TM.tm_get_hour (!p_tm)
  val tm_mday = $TM.tm_get_mday (!p_tm)
  val tm_mon = 1 + $TM.tm_get_mon (!p_tm)
  val tm_year = 1900 + $TM.tm_get_year (!p_tm)
//
  prval((*returned*)) = fpf1 (pf1)
//
in
//
$extfcall
(
  void
, "fprintf"
, out, "%i-%i-%i: %2ih:%2im\n", tm_year, tm_mon, tm_mday, tm_hour, tm_min
) (* $extfcall *)
//
end // end of [then]
else let
  prval None_v() = pfopt
in
  emit_text(out, "**TIME-ERROR**\n")
end // end of [else]
//
val () = emit_text (out, ";;\n")
val () = emit_text (out, ";;;;;;\n")
//
in
  // emit_newline (out)
end // end of [emit_time_stamp]

end // end of [local]

(* ****** ****** *)

implement
emit_PMVint
  (out, tok) = let
//
val-T_INT(base, rep) = tok.token_node
//
in
  emit_text (out, rep)
end // end of [emit_PMVint]
//
implement
emit_PMVintrep
  (out, tok) = let
//
val-T_INT(base, rep) = tok.token_node
//
in
  emit_text (out, rep)
end // end of [emit_PMVintrep]
//
(* ****** ****** *)

implement
emit_PMVbool
  (out, tfv) = (
//
emit_text
( out
, if tfv
    then "atscc2clj_true" else "atscc2clj_false"
  // end of [if]
) (* end of [emit_text] *)
//
) (* end of [emit_PMVbool] *)

(* ****** ****** *)

implement
emit_PMVstring
  (out, tok) = let
//
val-T_STRING(rep) = tok.token_node
//
in
  emit_text (out, rep)
end // end of [emit_PMVstring]

(* ****** ****** *)

implement
emit_PMVfloat
  (out, tok) = let
//
val-T_FLOAT(base, rep) = tok.token_node
//
in
  emit_text (out, rep)
end // end of [emit_PMVfloat]

(* ****** ****** *)

implement
emit_PMVi0nt
  (out, tok) = let
//
val-T_INT(base, rep) = tok.token_node
//
in
  emit_text (out, rep)
end // end of [emit_PMVi0nt]

(* ****** ****** *)

implement
emit_PMVf0loat
  (out, tok) = let
//
val-T_FLOAT(base, rep) = tok.token_node
//
in
  emit_text (out, rep)
end // end of [emit_PMVf0loat]

(* ****** ****** *)
//
implement
emit_PMVempty
  (out, _) = emit_text (out, "null")
//  
implement
emit_PMVextval
  (out, toks) = emit_tokenlst (out, toks)
//
(* ****** ****** *)
//
extern
fun
f0ide_get_arity (fid: i0de): int
//
implement
f0ide_get_arity
  (fid) = let
//
val
opt =
f0head_search_opt(fid.i0dex_sym)
//
in
//
case+ opt of
| ~None_vt() => ~1
| ~Some_vt(fhd) =>
  (
    case+
    fhd.f0head_node
    of // case+
    | F0HEAD(fid, fma, _) =>
        list_length (fma.f0marg_node)
      // end of [F0HEAD]
  ) (* end of [Some_vt] *)
//
end // end of [f0ide_get_arity]
//
(* ****** ****** *)
//
implement
emit_PMVfunlab
  (out, flab) =
{
//
(*
val n0 = f0ide_get_arity(flab)
*)
//
val () = emit_label(out, flab)
//
} (* end of [emit_PMVfunlab] *)
//
(* ****** ****** *)

implement
emit_PMVcfunlab
  (out, flab, d0es_env) =
{
//
val () = emit_LPAREN(out)
//
val () =
  emit_label(out, flab)
val () =
  emit_text (out, "__closurerize")
//
val () = emit_d0explst_1(out, d0es_env)
//
val () = emit_RPAREN(out)
//
} (* end of [emit_PMVcfunlab] *)

(* ****** ****** *)

implement
emit_CSTSPmyloc
  (out, tok) = let
//
val-T_STRING(rep) = tok.token_node
//
in
  emit_text (out, rep)
end // end of [emit_CSTSPmyloc]

(* ****** ****** *)
//
extern
fun
emit_fname_d0exp
  : (FILEref, string, d0exp) -> void
extern
fun
emit_fname_d0exp2
  : (FILEref, string, d0exp, d0exp) -> void
extern
fun
emit_fname_d0exp_int
  : (FILEref, string, d0exp, int(*ctag*)) -> void
//
implement
emit_fname_d0exp
  (out, fname, d0e) =
{
//
val () = emit_LPAREN (out)
//
val () = (
  emit_text (out, fname); emit_SPACE (out); emit_d0exp (out, d0e)
) (* end of [val] *)
//
val () = emit_RPAREN (out)
//
} (* end of [emit_fname_d0exp] *)
//
implement
emit_fname_d0exp2
  (out, fname, d0e1, d0e2) =
{
//
val () = emit_LPAREN (out)
val () =
(
  emit_text (out, fname); emit_SPACE (out); 
  emit_d0exp (out, d0e1); emit_SPACE (out); emit_d0exp (out, d0e2)
)
val () = emit_RPAREN (out)
//
} (* end of [emit_fname_d0exp2] *)
//
implement
emit_fname_d0exp_int
  (out, fname, d0e, ctag) =
{
//
val () = emit_LPAREN (out)
val () =
(
  emit_text (out, fname); emit_SPACE (out); 
  emit_d0exp (out, d0e); emit_SPACE (out); emit_int (out, ctag)
)
val () = emit_RPAREN (out)
//
} (* end of [emit_fname_d0exp_int] *)
//
(* ****** ****** *)
//
implement
emit_ATSCKiseqz(out, d0e) =
  emit_fname_d0exp(out, "ATSCKiseqz", d0e)
implement
emit_ATSCKisneqz(out, d0e) =
  emit_fname_d0exp(out, "ATSCKisneqz", d0e)
//
(* ****** ****** *)
//
implement
emit_ATSCKptriscons(out, d0e) =
  emit_fname_d0exp(out, "ATSCKptriscons", d0e)
implement
emit_ATSCKptrisnull(out, d0e) =
  emit_fname_d0exp(out, "ATSCKptrisnull", d0e)
//
(* ****** ****** *)
//
implement
emit_ATSCKpat_int
  (out, d0e, i0) =
  emit_fname_d0exp2 (out, "ATSCKpat_int", d0e, i0)
implement
emit_ATSCKpat_bool
  (out, d0e, b0) =
  emit_fname_d0exp2 (out, "ATSCKpat_bool", d0e, b0)
implement
emit_ATSCKpat_string
  (out, d0e, s0) =
  emit_fname_d0exp2 (out, "ATSCKpat_string", d0e, s0)
//
(* ****** ****** *)
//
implement
emit_ATSCKpat_con0
  (out, d0e, ctag) =
  emit_fname_d0exp_int (out, "ATSCKpat_con0", d0e, ctag)
implement
emit_ATSCKpat_con1
  (out, d0e, ctag) =
  emit_fname_d0exp_int (out, "ATSCKpat_con1", d0e, ctag)
//
(* ****** ****** *)
//
implement
emit_tmpvar
  (out, tmp) = let
//
val
sym = tmp.i0dex_sym
val
name =
g1ofg0(symbol_get_name(sym))
//
in
//
if
isneqz(name)
then (
  emit_text(out, name)
) (* end of [then] *)
else () // end of [else]
//
end // end of [emit_tmpvar]
//
(* ****** ****** *)

local

fun
skipds
(
  p1: ptr
) : ptr = let
  val c1 = $UN.ptr0_get<char>(p1)
in
  if isdigit(c1)
    then skipds(ptr_succ<char>(p1)) else p1
  // end of [if]
end // end of [skipds]

fun
emit_axrg__
(
  out: FILEref, sym: symbol
) : void = let
//
val p0 =
string2ptr
  (symbol_get_name(sym))
// string2ptr
val p1 =
  skipds(ptr_succ<char>(p0))
//
val p1_2 = ptr_add<char>(p1, 2)
//
in
  emit_text(out, "arg");
  emit_text(out, $UN.cast{string}(p1_2))
end // end of [emit_axrg__]

in (* in-of-local *)

implement
emit_tmpvar_val
  (out, tmp) = let
//
val sym = tmp.i0dex_sym
//
val isat = tmpvar_is_tmp(sym)
//
in
//
if
isat
then let
//
val () =
  emit_text(out, "@")
//
in
  emit_tmpvar(out, tmp)
end // end of [then]
else
(
//
if ~tmpvar_is_axrg(sym)
  then emit_tmpvar(out, tmp) else emit_axrg__(out, sym)
//
) (* end of [else] *)
//
end // end of [emit_tmpvar_val]

end // end of [local]

(* ****** ****** *)

fun
s0exp_get_arity
  (s0e: s0exp): int =
(
case+
s0e.s0exp_node
of // case+
| S0Elist(s0es) => list_length(s0es) | _ => ~1
) (* end of [s0exp_get_arity] *)

(* ****** ****** *)

implement
emit_d0exp
  (out, d0e0) = let
in
//
case+
d0e0.d0exp_node of
//
| D0Eide (tmp) => 
  {
    val () = emit_tmpvar_val(out, tmp)
  }
//
| D0Eappid(fid, d0es) =>
  {
    val () = emit_LPAREN(out)
//
    val () = emit_i0de(out, fid)
    val () = emit_d0explst_1(out, d0es)
//
    val () = emit_RPAREN(out)
  }
| D0Eappexp(d0e, d0es) =>
  {
    val () = emit_LPAREN(out)
//
    val () = emit_d0exp(out, d0e)
    val () = emit_d0explst_1(out, d0es)
//
    val () = emit_RPAREN(out)
  }
//
| D0Elist (d0es) =>
  {
    val () = emit_LPAREN (out)
    val () = emit_text (out, "D0Elist")
    val () = emit_d0explst_1 (out, d0es)
    val () = emit_RPAREN (out)
  }
//
| ATSPMVint (int) => emit_PMVint (out, int)
| ATSPMVintrep (int) => emit_PMVintrep (out, int)
//
| ATSPMVbool (tfv) => emit_PMVbool (out, tfv)
//
| ATSPMVfloat (flt) => emit_PMVfloat (out, flt)
//
| ATSPMVstring (str) => emit_PMVstring (out, str)
//
| ATSPMVi0nt (int) => emit_PMVi0nt (out, int)
| ATSPMVf0loat (flt) => emit_PMVf0loat (out, flt)
//
| ATSPMVempty (dummy) => emit_PMVempty (out, 0)
| ATSPMVextval (toklst) => emit_PMVextval (out, toklst)
//
| ATSPMVrefarg0 (d0e) => emit_d0exp (out, d0e)
| ATSPMVrefarg1 (d0e) => emit_d0exp (out, d0e)
//
| ATSPMVfunlab (fl) => emit_PMVfunlab (out, fl)
| ATSPMVcfunlab
    (_(*knd*), fl, d0es) => emit_PMVcfunlab (out, fl, d0es)
  // end of [ATSPMVcfunlab]
//
| ATSPMVcastfn
    (_(*fid*), _(*s0e*), arg) => emit_d0exp (out, arg)
//
| ATSCSTSPmyloc (tok) => emit_CSTSPmyloc (out, tok)
//
//
| ATSCKiseqz(d0e) => emit_ATSCKiseqz (out, d0e)
| ATSCKisneqz(d0e) => emit_ATSCKisneqz (out, d0e)
| ATSCKptriscons(d0e) => emit_ATSCKptriscons (out, d0e)
| ATSCKptrisnull(d0e) => emit_ATSCKptrisnull (out, d0e)
//
| ATSCKpat_int
    (d0e, int) => emit_ATSCKpat_int (out, d0e, int)
| ATSCKpat_bool
    (d0e, bool) => emit_ATSCKpat_bool (out, d0e, bool)
| ATSCKpat_string
    (d0e, string) => emit_ATSCKpat_string (out, d0e, string)
//
| ATSCKpat_con0
    (d0e, ctag) => emit_ATSCKpat_con0 (out, d0e, ctag)
| ATSCKpat_con1
    (d0e, ctag) => emit_ATSCKpat_con1 (out, d0e, ctag)
//
| ATSSELcon _ => emit_SELcon (out, d0e0)
| ATSSELrecsin _ => emit_SELrecsin (out, d0e0)
| ATSSELboxrec _ => emit_SELboxrec (out, d0e0)
| ATSSELfltrec _ => emit_text (out, "ATSSELfltrec(...)")
//
| ATSextfcall
    (_fun, _arg) => {
    val () = emit_i0de (out, _fun)
    val () = emit_d0exparg (out, _arg)
  } (* end of [ATSextfcall] *)
| ATSextmcall
    (_obj, _mtd, _arg) => {
//
    val () = emit_d0exp (out, _obj)
    val () = emit_DOT (out)
    val () = emit_d0exp (out, _mtd)
//
    val () = emit_d0exparg (out, _arg)
//
  } (* end of [ATSextmcall] *)
//
| ATSfunclo_fun
  (
    d0e_fun, s0e_arg, _(*res*)
  ) => (
    emit_fname_d0exp(out, "ATSfunclo_fun", d0e_fun)
  ) (* end of [ATSfunclo_fun] *)
//
| ATSfunclo_clo
  (
    d0e_fun, _(*arg*), _(*res*)
  ) => (
    emit_fname_d0exp(out, "ATSfunclo_fclo", d0e_fun)
  ) (* end of [ATSfunclo_clo] *)
//
end // end of [emit_d0exp]

(* ****** ****** *)

local

fun
loop
(
  out: FILEref, d0es: d0explst, i: int
) : void =
(
case+ d0es of
| list_nil () => ()
| list_cons (d0e, d0es) => let
    val () =
      if i > 0 then emit_SPACE(out)
    // end of [val]
  in
    emit_d0exp (out, d0e); loop (out, d0es, i+1)
  end // end of [list_cons]
)

in (* in-of-local *)

implement
emit_d0explst (out, d0es) = loop (out, d0es, 0)
implement
emit_d0explst_1 (out, d0es) = loop (out, d0es, 1)

end // end of [local]

(* ****** ****** *)

implement
emit_d0exparg
  (out, d0es) = 
{
//
val () = emit_LPAREN (out)
val () = emit_d0explst (out, d0es)
val () = emit_RPAREN (out)
//
} (* end of [emit_d0exparg] *)

(* ****** ****** *)
//
extern
fun
tyrec_labsel
  (tyrec: tyrec, lab: symbol): int
//
implement
tyrec_labsel
  (tyrec, lab) = let
//
fun loop
(
  xs: tyfldlst, i: int
) : int =
(
case+ xs of
| list_cons (x, xs) => let
    val TYFLD (id, s0e) = x.tyfld_node
  in
    if lab = id.i0dex_sym then i else loop (xs, i+1)
  end // end of [list_cons
| list_nil ((*void*)) => ~1(*error*)
)
//
in
  loop (tyrec.tyrec_node, 0)
end // end of [tyrec_labsel]
//
(* ****** ****** *)

implement
emit_SELcon
  (out, d0e) = let
//
val-
ATSSELcon
  (d0rec, s0e, id) = d0e.d0exp_node
//
val-S0Eide(name) = s0e.s0exp_node
val-~Some_vt(s0rec) = typedef_search_opt(name)
//
val tupi = tyrec_labsel(s0rec, id.i0dex_sym)
//
val () =
  emit_text(out, "(ATSSELcon ")
val () =
(
  emit_d0exp (out, d0rec); emit_SPACE(out); emit_int (out, tupi)
) (* end of [val] *)
//
val () = emit_RPAREN (out)
//
in
  // nothing
end // end of [emit_SELcon]

(* ****** ****** *)

implement
emit_SELrecsin
  (out, d0e) = let
//
val-
ATSSELrecsin
  (d0rec, s0e, id) = d0e.d0exp_node
//
in
  emit_d0exp (out, d0rec)
end // end of [emit_SELrecsin]

(* ****** ****** *)

implement
emit_SELboxrec
  (out, d0e) = let
//
val-
ATSSELboxrec
  (d0rec, s0e, id) = d0e.d0exp_node
//
val-S0Eide(name) = s0e.s0exp_node
val-~Some_vt(s0rec) = typedef_search_opt(name)
//
val tupi = tyrec_labsel(s0rec, id.i0dex_sym)
//
val () =
emit_text
  (out, "(ATSSELboxrec ")
//
//
val () =
(
  emit_d0exp (out, d0rec); emit_SPACE(out); emit_int (out, tupi)
) (* end of [val] *)
//
val () = emit_RPAREN (out)
//
in
  // nothing
end // end of [emit_SELboxrec]

(* ****** ****** *)
//
implement
emit_COMMENT_line
  (out, tok) = let
//
val-
T_COMMENT_line
  (str) = tok.token_node
//
in
  emit_text (out, str)
end // end of [emit_COMMENT_line]
//
implement
emit_COMMENT_block
  (out, tok) = let
//
val-
T_COMMENT_block
  (str) = tok.token_node
//
in
  emit_text (out, str)
end // end of [emit_COMMENT_block]
//
(* ****** ****** *)

local

(*
fun
aux0_cenv
(
  out: FILEref
, s0es: s0explst
) : void = let
//
fun
auxlst
(
  i: int, s0es: s0explst
) : void =
(
case+ s0es of
| list_nil() => ()
| list_cons
    (_, s0es) => let
    val () =
      emit_text(out, " ")
    val () =
    (
      emit_text(out, "Cenv"); emit_int(out, i)
    )
  in
    auxlst(i+1, s0es)
  end // end of [auxlst]
)
//
val () = emit_LBRACE(out)
val () = (emit_text(out, "_"); auxlst(1, s0es))
val () = emit_RBRACE(out)
//
in
  // nothing
end (* end of [aux0_cenv] *)
*)

fun
aux0_cenv
(
  out: FILEref
, s0es: s0explst
) : void =
(
  emit_text(out, "_fcenvs_")
)

fun
aux0_arglst
(
  out: FILEref
, s0es: s0explst
, n0: int, i: int
) : void = (
//
case+ s0es of
| list_nil
    ((*void*)) => ()
| list_cons
    (s0e, s0es) => let
    val () =
    if n0+i > 0
      then (
        emit_text (out, " ")
      ) (* then *)
    // end of [if]
    val () =
    (
      emit_text (out, "xarg"); emit_int (out, i)
    ) (* end of [val] *)
  in
    aux0_arglst (out, s0es, n0, i+1)
  end // end of [list_cons]
//
) (* end of [aux0_arglst] *)

fun
aux0_envlst
(
  out: FILEref
, s0es: s0explst
, n0: int, i: int
) : void = (
//
case+ s0es of
| list_nil () => ()
| list_cons
    (s0e, s0es) => let
    val () =
    if n0+i > 0
      then (
        emit_text(out, " ")
      ) (* then *)
    // end of [if]
    val () =
    (
      emit_text (out, "xenv"); emit_int (out, i)
    ) (* end of [val] *)
  in
    aux0_envlst (out, s0es, n0, i+1)
  end // end of [list_cons]
//
) (* end of [aux0_envlst] *)

fun
aux0_fcenvs_at
(
  out: FILEref
, s0es: s0explst, i: int
) : void = () where
{
  val () = emit_LPAREN(out)
  val () =
  if (i <= 3)
    then (
      emit_text(out, "ATSCCget_"); emit_int(out, i); emit_text(out, " _fcenvs_")
    ) (* end of [then] *)
    else (
      emit_text(out, "ATSCCget_at"); emit_text(out, " _fcenvs_ "); emit_int(out, i)
    ) (* end of [else] *)
  // end of [if]
  val () = emit_RPAREN(out)
} (* end of [aux0_fcenvs_at] *)

fun
aux1_envlst
(
  out: FILEref
, s0es: s0explst, i: int
) : int =
(
case+ s0es of
| list_nil
    ((*void*)) => (i)
| list_cons
    (s0e, s0es) => let
//
    val () =
    if i > 0 then emit_text (out, " ")
    val () = aux0_fcenvs_at(out, s0es, i)
//
  in
    aux1_envlst (out, s0es, i+1)
  end // end of [list_cons]
) (* end of [aux1_envlst] *)

in (* in-of-local *)

implement
emit_closurerize
(
  out, flab, env, arg, res
) = let
//
val-S0Elist(s0es_env) = env.s0exp_node
val-S0Elist(s0es_arg) = arg.s0exp_node
//
val () = emit_ENDL (out)
//
(*
val () =
emit_text (out, ";;fun;;\n")
*)
val () =
emit_text(out, "(defn\n")
//
val () =
  emit_label(out, flab)
val () =
  emit_text (out, "__closurerize")
//
val () = emit_LBRACKET(out)
val () = aux0_envlst(out, s0es_env, 0, 0)
val () = emit_RBRACKET(out)
//
val () = emit_text(out, "\n;;%{\n")
//
val () = emit_nspc (out, 2)
val () = emit_text (out, "(list ")
val () = emit_text (out, "(fn")
//
val () =
  emit_LBRACKET(out)
val () = aux0_cenv(out, s0es_env)
val () = aux0_arglst (out, s0es_arg, 1, 0)
val () =
  emit_RBRACKET(out)
//
val () = emit_LPAREN(out)
val () = emit_label(out, flab)
val n0 = aux1_envlst(out, s0es_env, 1)
val () = aux0_arglst(out, s0es_arg, n0, 0)
val () = emit_RPAREN(out)
//
val () = emit_RPAREN(out)
//
val () = aux0_envlst(out, s0es_env, 1, 0)
val ((*closing*)) = emit_text(out, ")\n")
val ((*closing*)) = emit_text(out, ";;%}\n")
val ((*closing*)) = emit_text(out, ") ;; end-of-defn\n")
//
val ((*flushing*)) = emit_newline(out)
//
in
  // nothing
end // end of [emit_closurerize]

end // end of [local]

(* ****** ****** *)

(* end of [atscc2clj_emit.dats] *)

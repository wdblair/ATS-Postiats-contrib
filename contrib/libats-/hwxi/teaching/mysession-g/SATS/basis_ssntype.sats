(*
** Basis for g-session types
*)

(* ****** ****** *)
//
staload
"./basis_intset.sats"
//
(* ****** ****** *)
//
fun{}
channel_cap(): intGte(1)  
//
(* ****** ****** *)
//
abstype
session_msg
  (i:int, j:int, a:vt@ype)
//
(* ****** ****** *)

abstype ssession_nil
abstype ssession_cons(a:type, ssn:type)

(* ****** ****** *)
//
stadef msg = session_msg
//
stadef nil = ssession_nil
//
stadef :: = ssession_cons
stadef cons = ssession_cons
//
(* ****** ****** *)
//
abstype
session_choose
(
  i:int, ssn1:type, ssn2:type
) (* session_choose *)
//
stadef choose = session_choose
//
(* ****** ****** *)
//
absvtype
channel1_vtype
  (G:iset, n:int, ssn:type) = ptr
//
vtypedef
channel1
  (G:iset, n:int, ssn:type) = channel1_vtype(G, n, ssn)
//
vtypedef
cchannel1
  (G:iset, n:int, ssn:type) = channel1_vtype(ncomp(n, G), n, ssn)
//
(* ****** ****** *)
//
fun{}
channel1_get_nrole
  {n:int}{ssn:type}{G:iset}
  (chan: !channel1(G, n, ssn)): int(n)
//
fun{}
channel1_get_group
  {n:int}{ssn:type}{G:iset}
  (chan: !channel1(G, n, ssn)): intset(n,G)
//
(* ****** ****** *)
//
fun
{a:vt0p}
channel1_close
  {n:int}{ssn:type}{G:iset}(chan: channel1(G, n, nil)): void
//
(* ****** ****** *)
//
fun{}
channel1_skipin
  {a:vt0p}
  {n:int}{ssn:type}{G:iset}
  {i,j:nat | ismbr(G, i); ismbr(G, j)}
(
  !channel1(G, n, msg(i, j, a)::ssn) >> channel1(G, n, ssn)
) : void // end-of-function
praxi
lemma_channel1_skipin
  {a:vt0p}
  {n:int}{ssn:type}{G:iset}
  {i,j:nat | ismbr(G, i); ismbr(G, j)}
(
  !channel1(G, n, msg(i, j, a)::ssn) >> channel1(G, n, ssn)
) : void // lemma_channel1_skipin
//
fun{}
channel1_skipex
  {a:vt0p}
  {n:int}{ssn:type}{G:iset}
  {i,j:nat | ~ismbr(G, i); ~ismbr(G, j)}
(
  !channel1(G, n, msg(i, j, a)::ssn) >> channel1(G, n, ssn)
) : void // end-of-function
praxi
lemma_channel1_skipex
  {a:vt0p}
  {n:int}{ssn:type}{G:iset}
  {i,j:nat | ~ismbr(G, i); ~ismbr(G, j)}
(
  !channel1(G, n, msg(i, j, a)::ssn) >> channel1(G, n, ssn)
) : void // lemma_channel1_skipex
//
(* ****** ****** *)
//
fun
{a:vt0p}
channel1_send
  {n:int}{ssn:type}{G:iset}
  {i,j:nat | i < n; j < n; ismbr(G, i); ~ismbr(G, j)}
(
  !channel1(G, n, msg(i, j, a)::ssn) >> channel1(G, n, ssn), int(i), int(j), a
) : void // end of [channel1_send]
//
fun
{a:vt0p}
channel1_recv
  {n:int}{ssn:type}{G:iset}
  {i,j:nat | i < n; j < n; ~ismbr(G, i); ismbr(G, j)}
(
  !channel1(G, n, msg(i, j, a)::ssn) >> channel1(G, n, ssn), int(i), int(j), &a? >> a
) : void // end of [channel1_recv]
//
fun
{a:vt0p}
channel1_recv_val
  {n:int}{ssn:type}{G:iset}
  {i,j:nat | i < n; j < n; ~ismbr(G, i); ismbr(G, j)}
  (!channel1(G, n, msg(i, j, a)::ssn) >> channel1(G, n, ssn), int(i), int(j)): (a)
//
(* ****** ****** *)
//
datatype
choosetag
(
  a:type, b:type, c:type
) =
  | choosetag_l(a, b, a) of ()
  | choosetag_r(a, b, b) of ()
//
(* ****** ****** *)

fun{}
channel1_choose_l
  {n:int}
  {ssn1,ssn2:type}
  {G:iset}
  {i:nat | i < n; ismbr(G, i)}
(
  !channel1(G, n, choose(i,ssn1,ssn2)) >> channel1(G, n, ssn1), i: int(i)
) : void // end of [channel1_choose_l]
fun{}
channel1_choose_r
  {n:int}
  {ssn1,ssn2:type}
  {G:iset}
  {i:nat | i < n; ismbr(G, i)}
(
  !channel1(G, n, choose(i,ssn1,ssn2)) >> channel1(G, n, ssn2), i: int(i)
) : void // end of [channel1_choose_r]

(* ****** ****** *)

fun{}
channel1_choosetag
  {n:int}
  {ssn1,ssn2:type}
  {G:iset}
  {i:nat | i < n; ~isnil(G); ~ismbr(G, i)}
(
  !channel1(G, n, choose(i,ssn1,ssn2)) >> channel1(G, n, ssn), i: int(i)
) : #[ssn:type] choosetag(ssn1, ssn2, ssn)

(* ****** ****** *)
//
(*
//
// HX-2015-03-06:
// This one does not work with sschoose!!!
//
fun{}
channel1_link
  {n:int}{ssn:type}
  {G1,G2:iset | isnil(G1*G2)}
  (channel1(G1, n, ssn), channel1(G2, n, ssn)): channel1(G1+G2, n, ssn)
*)
//
fun{}
channel1_link
  {n:int}{ssn:type}
  {G1,G2:iset | isful(G1+G2,n)}
  (channel1(G1, n, ssn), channel1(G2, n, ssn)): channel1(G1*G2, n, ssn)
//
(* ****** ****** *)
//
fun{}
channel1_link_elim
  {n:int}{ssn:type}{G:iset}(channel1(G, n, ssn), cchannel1(G, n, ssn)): void
//
(* ****** ****** *)
//
fun{}
cchannel1_create_exn
  {n:nat}{ssn:type}{G:iset}
(
  nrole: int(n), G: intset(n), fserv: channel1(G, n, ssn) -<lincloptr1> void
) : cchannel1(G, n, ssn) // end of [cchannel1_create_exn]
//
(* ****** ****** *)

(* end of [basis_ssntype.sats] *)

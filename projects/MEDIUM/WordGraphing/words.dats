(* ****** ****** *)

(*
** Author: Zoe Xi
** Author: Hongwei Xi
*)

(* ****** ****** *)
//
#include
"share/atspre_staload.hats"
#include
"share/HATS/atspre_staload_libats_ML.hats"
//
(* ****** ****** *)

staload "./words.sats"

(* ****** ****** *)

implement
theDictionary =
let
//
val INITCAP = 1024
//
in
  hashtbl_make_nil<string,word>(i2sz(INITCAP))
end // end of [theDictionary]

(* ****** ****** *)

implement
word_create
  (spelling) = w0 where
{
//
val w0 =
  gvhashtbl_make_nil(16)
val () =
  w0["spelling"] := GVstring(spelling)
//
} (* word_create *)

(* ****** ****** *)

implement
word_create_add
  (spelling) = w0 where
{
//
val w0 =
  word_create(spelling)
val-~None_vt() =
  hashtbl_insert(theDictionary, spelling, w0)
} (* word_create_add *)

(* ****** ****** *)

implement main0 () = ((*void*))

(* ****** ****** *)

(* end of [words.dats] *)

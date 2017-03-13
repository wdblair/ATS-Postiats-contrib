staload "libats/ML/SATS/list0.sats"
staload "libats/ML/DATS/list0.dats"
staload "libats/ML/DATS/hashtblref.dats"
staload "libats/DATS/hashtbl_chain.dats"
staload "libats/DATS/linmap_list.dats"

#define ::  list_cons
#define nil list_nil

typedef map(key:t@ype, itm: t@ype) = $HTR.hashtbl(key, itm)

local

  val d2map = ref<map(stamp, d2var)>($HTR.hashtbl_make_nil<stamp, d2var>(i2sz(100)))

in

implement
parse_d2varmap(d2jsmap: jsonval): void = let
  val dvs = $HTR.hashtbl_make_nil<stamp, d2var>(i2sz(100))
  val- JSONarray (dlst) = d2jsmap
in
  list0_foreach<jsonval>(list0_of_list(dlst), lam (djs) => {
    val d2v   = parse_d2var(djs)
    val stamp = d2var_get_stamp(d2v)
    val ()    = $HTR.hashtbl_insert_any(!d2map, stamp, d2v)
  })
end

local

typedef
d2var_struct = @{
    d2var_sym= symbol,
    d2var_stamp= stamp
}

assume
d2var_type =
  ref(d2var_struct)

in

  implement
  parse_d2var(js) = let
    val- JSONobject(lablst) = js
    val- (_, JSONstring(symbol)) :: (_, JSONint(stamp)) :: _ = lablst
  in
    ref(@{
      d2var_sym=symbol_make_name(symbol),
      d2var_stamp=stamp_make($UN.cast{int}(stamp))
    })
  end

  implement
  d2var_get_stamp(d2v) = !d2v.d2var_stamp

  implement
  d2var_get_symbol(d2v) = !d2v.d2var_sym

end

implement
d2var_of_stamp_exn(s) = let
  val opt = $HTR.hashtbl_search(!d2map, s)
in
  case+ opt of
    | ~None_vt () => $raise D2varNotDefined(s)
    | ~Some_vt d2 => d2
end

end
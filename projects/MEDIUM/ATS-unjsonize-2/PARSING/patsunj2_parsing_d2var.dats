staload "libats/ML/SATS/list0.sats"
staload "libats/ML/DATS/list0.dats"
staload "libats/ML/DATS/hashtblref.dats"
staload "libats/DATS/hashtbl_chain.dats"
staload "libats/DATS/linmap_list.dats"

typedef map(key:t@ype, itm: t@ype) = $HTR.hashtbl(key, itm)

local

  val d2map = ref<map(stamp, d2var)>($HTR.hashtbl_make_nil<stamp, d2var>(i2sz(100)))

in

fun
parse_d2varmap(json: jsonval): void = let
  val- JSONobject(lablst) = json

  val d2jsmap = list0_find_exn(list0_of_list(lablst), lam (jslab) => let
    val- @(label, js) = jslab
  in
    label = "d2varmap"
  end)

  val dvs = $HTR.hashtbl_make_nil<stamp, d2var>(i2sz(100))
  val- JSONarray (dlst) = json
in
  list0_foreach<jsonval>(list0_of_list(dlst), lam (djs) => {
    val d2v   = parse_d2var(djs)
    val stamp = d2var_get_stamp(d2v)
    val ()    = $HTR.hashtbl_insert_any(!d2map, stamp, d2v)
  })
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
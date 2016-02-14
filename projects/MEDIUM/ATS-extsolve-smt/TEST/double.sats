staload "./float.sats"

abst@ype double (f: float) = $extype "double"

fun
abs {f:float} (
  x: double (f)
): double (abs_float(f)) = "mac#%"

fun
neg_double {f:float} (
  x: double (f)
): double (neg_float(f)) = "mac#%"

overload ~ with neg_double

fun
add_double_double {x,y:float} (
  x: double (x), y: double (y)
): double (x + y) = "mac#%"

overload + with add_double_double 

fun
sub_double_double {x,y:float} (
  x: double (x), y: double (y)
): double(x-y) = "mac#%"

overload - with sub_double_double

fun
mul_double_double {x,y:float} (
  x: double (x), y: double (y)
): double(x*y) = "mac#%"

overload * with mul_double_double

fun
div_double_double {x,y:float} (
  x: double (x), y: double (y)
): double(x*y) = "mac#%"

overload / with div_double_double

fun
rem_double_double {x,y:float} (
  x: double (x), y: double (y)
): double(x mod y) = "mac#%"

overload mod with rem_double_double

fun
fma_double_double {x,y,z:float} (
  x: double (x), y: double (y), z: double(z)
): double (fma_float_float_float(x,y,z)) = "mac#%"

fun
lt_double_double {x,y:float} (
  x: double (x), y: double (y)
): bool (x < y) = "mac#%"

overload < with lt_double_double

fun
lte_double_double {x,y:float} (
  x: double (x), y: double (y)
): bool (x <= y) = "mac#%"

overload <= with lte_double_double

fun
gt_double_double {x,y:float} (
  x: double (x), y: double (y)
): bool (x > y) = "mac#%"

overload > with gt_double_double

fun
gte_double_double {x,y:float} (
  x: double (x), y: double (y)
): bool (x >= y) = "mac#%"

overload >= with gte_double_double

fun
eq_double_double {x,y:float} (
  x: double (x), y: double (y)
): bool (x == y) = "mac#%"

overload = with eq_double_double

fun
neq_double_double {x,y:float} (
  x: double (x), y: double (y)
): bool (x != y) = "mac#%"

overload != with neq_double_double

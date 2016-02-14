staload UN = "prelude/SATS/unsafe.sats"

staload "double.sats"

#define
PI 3.1415926536
//
val PI_ =
  $UN.cast{double(fp64(PI))}(PI)
//
val PI_sqrt = sqrt_double(PI_)

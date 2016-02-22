datasort float64 = (* abstract *)

stacst abs_float64 : (float64) -> float64
stadef abs = abs_float64

stacst neg_float64 : (float64) -> float64
stadef neg = neg_float64

stacst add_float64_float64 : (float64, float64) -> float64
stadef + = add_float64_float64

stacst sub_float64_float64 : (float64, float64) -> float64
stadef - = sub_float64_float64

stacst mul_float64_float64 : (float64, float64) -> float64
stadef * = mul_float64_float64

stacst div_float64_float64 : (float64, float64) -> float64
stadef / = div_float64_float64

stacst rem_float64_float64 : (float64, float64) -> float64
stadef mod = rem_float64_float64

stacst fma_float64_float64_float64 : (float64, float64, float64) -> float64

stacst sqrt_float64 : float64 -> float64
stadef sqrt = sqrt_float64

stacst lt_float64_float64 : (float64, float64) -> bool
stadef < = lt_float64_float64

stacst lte_float64_float64 : (float64, float64) -> bool
stadef <= = lte_float64_float64

stacst gt_float64_float64 : (float64, float64) -> bool
stadef > = gt_float64_float64

stacst gte_float64_float64 : (float64, float64) -> bool
stadef >= = gte_float64_float64

stacst eq_float64_float64 : (float64, float64) -> bool
stadef == = eq_float64_float64

stacst neq_float64_float64 : (float64, float64) -> bool
stadef != = neq_float64_float64

stacst float64_round : float64 -> float64

stacst float64_is_normal : float64 -> bool
stacst float64_is_subnormal : float64 -> bool
stacst float64_is_zero : float64 -> bool
stacst float64_is_infinite : float64 -> bool
stacst float64_is_nan : float64 -> bool
stacst float64_is_negative : float64 -> bool
stacst float64_is_positive : float64 -> bool

stacst float_to_float64: float -> float64
stadef fp64 = float_to_float64

stacst float64_to_float: float64 -> float
stadef to_real = float64_to_float

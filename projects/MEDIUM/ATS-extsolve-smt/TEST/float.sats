stacst abs_float : (float) -> float
stacst neg_float : (float) -> float

stacst add_float_float : (float, float) -> float
stadef + = add_float_float

stacst sub_float_float : (float, float) -> float
stadef - = sub_float_float

stacst mul_float_float : (float, float) -> float
stadef * = mul_float_float

stacst div_float_float : (float, float) -> float
stadef / = div_float_float

stacst rem_float_float : (float, float) -> float
stadef mod = rem_float_float

stacst fma_float_float_float : (float, float, float) -> float

stacst lt_float_float : (float, float) -> bool
stadef < = lt_float_float

stacst lte_float_float : (float, float) -> bool
stadef <= = lte_float_float

stacst gt_float_float : (float, float) -> bool
stadef > = gt_float_float

stacst gte_float_float : (float, float) -> bool
stadef >= = gte_float_float

stacst eq_float_float : (float, float) -> bool
stadef == = eq_float_float

stacst neq_float_float : (float, float) -> bool
stadef != = neq_float_float

stacst float_round : float -> float

stacst float_is_normal : float -> bool
stacst float_is_subnormal : float -> bool
stacst float_is_zero : float -> bool
stacst float_is_infinite : float -> bool
stacst float_is_nan : float -> bool
stacst float_is_negative : float -> bool
stacst float_is_positive : float -> bool
(** Defining Operations of Natural Numbers
    Derived from the leon smt-lib benchmarks *)
datasort Nat =
  | zero of ()
  | succ of (Nat)
  
stacst less_nat_nat: (Nat, Nat) -> bool
stadef < = less_nat_nat

praxi
less_base_lemma (): [~(zero() < zero())] unit_p

praxi
less_zero_lemma {x:Nat} (): [zero() < succ(x)] unit_p

praxi
less_ind_lemma {x,y:Nat} (): [(succ(x) < succ(y)) == (x < y)] unit_p

stadef leq(x:Nat, y:Nat) = ((x == y) || (x < y))

stadef <= = leq

stadef gt_nat_nat(x:Nat, y:Nat) = ~(x <= y)
stadef > = gt_nat_nat

stacst plus_nat_nat: (Nat, Nat) -> Nat
stadef + = plus_nat_nat

praxi
plus_base_lemma {x:Nat} (): [(zero + x) == x] unit_p

praxi
plus_ind_lemma {n,m:Nat} (): [succ(n) + m == succ (n + m)] unit_p

stacst nat_to_int: (Nat) -> int
stadef int = nat_to_int

praxi
nats_lower_bound_lemma {x:Nat} (): [int(x) >= 0] unit_p

praxi
nats_equality_lemma {x,y:Nat | int(x) == int(y)} (): [x == y] unit_p

praxi
nats_zero_lemma (): [int zero == 0] unit_p

praxi
nats_succ_lemma {x:Nat} (): [int(succ (x)) == 1 + int(x)] unit_p

praxi
nats_less_lemma {x,y:Nat} (): [x < y == (int(x) < int(y))] unit_p

praxi
nats_leq_lemma {x,y:Nat} (): [x <= y == (int(x) <= int(y))] unit_p

praxi
nats_plus_lemma {x,y:Nat} (): [int(x + y) == int(x) + int(y)] unit_p

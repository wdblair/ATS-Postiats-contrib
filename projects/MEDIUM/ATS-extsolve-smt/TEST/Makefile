all:: test

show: glist.dats
	patsopt --constraint-export -tc -d $^ | ../patsolve_smt -i

test: glist.dats
	patsopt --constraint-export -tc -d $^ | ../patsolve_smt -i | cvc4+ig --lang smt2

z3: glist.dats
	patsopt --constraint-export -tc -d $^ | ../patsolve_smt -i | z3 -smt2 -in

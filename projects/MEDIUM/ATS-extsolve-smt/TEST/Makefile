all:: test

show: glist.dats
	patsopt --constraint-export -tc -d $^ | patsolve_smt -i

test: glist.dats
	patsopt --constraint-export -tc -d $^ | patsolve_smt -i | cvc4+ig --lang smt


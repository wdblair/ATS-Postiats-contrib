all:: show-double double

show: glist.dats
	patsopt --constraint-export -tc -d $^ | ../patsolve_smt -i

show-double: double.dats
	patsopt --constraint-export -tc -d $^ | ../patsolve_smt -i

double: double.dats
	patsopt --constraint-export -tc -d $^ | ../patsolve_smt -i | z3 -smt2 -in
	patscc --constraint-ignore double.dats -o double

# build system doesn't have CVC4 at the moment
cvc4: glist.dats
	patsopt --constraint-export -tc -d $^ | ../patsolve_smt -i | cvc4+ig --lang smt2

z3: glist.dats
	patsopt --constraint-export -tc -d $^ | ../patsolve_smt -i | z3 -smt2 -in

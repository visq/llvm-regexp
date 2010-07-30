all: Grep

Grep: Grep.hs RegExpLLVM.hs Parser.y RegExp.hs
	happy Parser.y
	ghc -O2 --make Grep.hs

smoke: RegExpLLVM
	./RegExpLLVM '((a|b)*c(a|b)*c)*(a|b)*' < tests/evencs_smoke.in | diff tests/evencs_smoke.expect --to=-

check: 	matcher.bc
	llvm-dis matcher.bc
	opt matcher.bc -verify -stats

matcher.bc: RegExpLLVM
	(echo "" | ./RegExpLLVM) || echo "[ignore return value of RegExpLLVM]"
	
# Old build script, used before JIT worked
# ./RegExpLLVM
# llvm-as main.ll
# opt matcher.bc -O3 -f -o matcher.bc
# llvm-dis matcher.bc
# llvm-ld  -native -o matcher matcher.bc main.o
# llvm-ld -o matcher.i main.bc matcher.bc 

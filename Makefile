all: RegExpLLVM

RegExpLLVM: RegExpLLVM.hs
	ghc -O2 --make RegExpLLVM.hs

smoke: RegExpLLVM
	./RegExpLLVM < tests/evencs_smoke.in | diff tests/evencs_smoke.expect --to=-


# Old build script, used before JIT worked
# ./RegExpLLVM
# llvm-as main.ll
# opt matcher.bc -O3 -f -o matcher.bc
# llvm-dis matcher.bc
# llvm-ld  -native -o matcher matcher.bc main.o
# llvm-ld -o matcher.i main.bc matcher.bc 

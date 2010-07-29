all: RegExpLLVM
	./RegExpLLVM < test_evencs_smoke.in
	llvm-as main.ll
	opt matcher.bc -O3 -f -o matcher.bc
	llvm-dis matcher.bc
	llvm-ld  -native -o matcher matcher.bc main.o
	# llvm-ld -o matcher.i main.bc matcher.bc 
RegExpLLVM: RegExpLLVM.hs
	ghc -O2 --make RegExpLLVM.hs

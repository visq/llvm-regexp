all: RegExpLLVM
	./RegExpLLVM || echo "Continuing"
	llvm-as main.ll
	opt matcher.bc -O3 -f -o matcher.bc
	llvm-dis matcher.bc
	llvm-ld  -native -o matcher matcher.bc main.bc
	# llvm-ld -o matcher.i main.bc matcher.bc 
RegExpLLVM: RegExpLLVM.hs
	ghc -O --make RegExpLLVM.hs

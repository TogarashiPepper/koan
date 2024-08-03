buildir:
	clang stdlib.c -S -emit-llvm -O3

buildtest:
	clang stdtest.c -O3

analyze:
	scan-build clang stdlib.c -S -emit-llvm -O3
	scan-build clang stdtest.c -S -emit-llvm -O3
	@$(MAKE) clean

clean:
	-@rm stdlib.ll 2>/dev/null
	-@rm a.out 2>/dev/null

check:
	clang -fsyntax-only -Wall -Wextra stdlib.c

test: buildtest
	-./a.out
	@$(MAKE) clean

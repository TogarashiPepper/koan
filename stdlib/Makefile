buildir:
	clang stdlib.c -S -emit-llvm -O3

buildtest:
	clang stdtest.c -O3

analyze:
	scan-build clang stdlib.c -S -emit-llvm -O3
	@$(MAKE) clean

clean:
	-@rm stdlib.ll 2>/dev/null
	-@rm a.out 2>/dev/null

test: buildtest
	-./a.out
	@$(MAKE) clean

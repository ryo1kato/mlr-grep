ifeq ($(MAKECMDGOALS),prof)
    OPTS = -rtsopts -prof -auto-all
else
ifeq ($(MAKECMDGOALS),static)
    OPTS = -static -optl-static -optl-pthread
else
	OPTS = -O2 -Wall -fno-warn-missing-signatures
endif
endif

all: hmlgrep
prof static: all

perftest: test-result/test.data
	./perftest.sh

test-result/test.data:
	mkdir -p test-result
	./test/gentestlog.sh

hmlgrep: hmlgrep.hs ByteStr.hs Makefile
	ghc --make $< $(OPTS)

clean:
	rm -f hmlgrep.prof
	rm -f *.o
	rm -f *.hi
	rm -f test-result/*grep
	rm -f test-result/*grep.ret
	rm -f hmlgrep

cleanall:
	rm -rf test-result

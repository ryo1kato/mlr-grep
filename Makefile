ifeq ($(MAKECMDGOALS),prof)
    OPTS = -rtsopts -prof -auto-all
else
ifeq ($(MAKECMDGOALS),static)
    OPTS = -static -optl-static -optl-pthread
else
    OPTS = -O2 -Wall -fno-warn-missing-signatures
endif
endif
GHC = ghc -cpp --make

all: hmlgrep gmlgrep rmlgrep
prof static: all

perftest: test-result/test.data
	./perftest.sh

test-result/test.data:
	mkdir -p test-result
	./test/gentestlog.sh

hmlgrep: hmlgrep.hs Makefile
	$(GHC) $< $(OPTS)

rmlgrep: rust/src/main.rs
	cd rust && cargo build --release
	ln -sf rust/target/release/rmlgrep ./rmlgrep

gmlgrep: golang/gmlgrep.go golang/bufscan.go
	cd golang && go build $(notdir $^)
	ln -sf golang/gmlgrep ./gmlgrep

clean:
	rm -f hmlgrep.prof
	rm -f *.o
	rm -f *.hi
	rm -f test-result/*grep
	rm -f test-result/*grep.ret
	rm -f hmlgrep
	cd rust && cargo clean
	rm -f rmlgrep
	rm -f golang/gmlgrep
	rm -f gmlgrep

cleanall:
	rm -rf test-result

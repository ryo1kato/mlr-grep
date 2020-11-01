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
JULIA = /Applications/Julia-1.5.app/Contents/Resources/julia/bin/julia

all: hmlgrep gmlgrep rmlgrep
prof static: all

perftest: test-result/test.data
	./perftest.sh

test-result/test.data:
	mkdir -p test-result
	./test/gentestlog.sh

jmlgrep:
	cd julia && $(JULIA) --project -e 'using PackageCompiler; create_app(".", "build"; force=true, incremental=true)'

hmlgrep: hmlgrep.hs Makefile
#	$(GHC) $< $(OPTS)
	stack build --copy-bins --local-bin-path $(CURDIR)

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
	stack clean
	rm -rf test-result

.PHONY: jmlgrep

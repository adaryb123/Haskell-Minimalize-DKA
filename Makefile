OUT := flp21-fun

.PHONY: build
build: $(OUT)

$(OUT): src/*.hs
	ghc -Wall src/*.hs --make -o flp21-fun


.PHONY: clean
clean:
	rm -f src/*.hi src/*.o flp21-fun
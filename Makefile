chap = chap2

all: $(chap)

$(chap): % : %.hs
	ghc $<

test: $(chap)
	@for exe in $(chap);do echo $$exe && ./$$exe; done

clean:
	rm -rf $(chap) $(chap).hi $(chap).o

cloodoo: src/*.lisp *.asd
	sbcl --eval "(asdf:make :cloodoo)" --quit

clean:
	rm -rf *~ cloodoo

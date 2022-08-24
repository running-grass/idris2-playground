.PHONY: help
help:
	echo "help: print this \ntest: run test for this project \nbuild: build idris library \nrun: run it if can"

.PHONY: test
test: 
	pack run test/test.ipkg


.PHONY: build
build: 
	pack build lookhere.ipkg


.PHONY: run
run: build
	node build/exec/lookhere

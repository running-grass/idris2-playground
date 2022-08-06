.PHONY: run
run:
	pack run douban/douban.ipkg


.PHONY: build
build: 
	pack build douban/douban.ipkg

.PHONY: test
test: 
	pack run test/test.ipkg

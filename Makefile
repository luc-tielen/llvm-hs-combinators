build:
	stack build --fast

test:
	stack test --fast

clean:
	rm -rf .stack-work

.PHONY: build test clean

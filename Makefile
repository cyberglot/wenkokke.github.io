.PHONY: init
init:
	git config core.hooksPath .githooks

.PHONY: build
build:
	stack build && stack run build

.PHONY: clean
clean:
	stack build && stack run clean

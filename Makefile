.PHONY: init
init:
	git config core.hooksPath .githooks

.PHONY: build
build:
	stack build && stack run build

.PHONY: watch
watch:
	stack build && stack run watch

.PHONY: clean
clean:
	stack build && stack run clean

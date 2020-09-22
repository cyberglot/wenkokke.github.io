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

.PHONY: deploy
deploy:
	git stash
	git checkout dev
	make clean
	make build
	git fetch --all
	git checkout -b main --track origin/main
	rsync -a \
		--filter='P _site/'      \
		--filter='P _cache/'     \
		--filter='P .git/'       \
		--filter='P .gitignore'  \
		--filter='P .stack-work' \
		--delete-excluded        \
		_site/ .
	git add -A
	git commit -m "Publish."
	git push origin main:main
	git checkout dev
	git branch -D main
	git stash pop

########################################
# Setup Git Hooks
########################################

.PHONY: init
init:
	git config core.hooksPath .githooks

########################################
# Setup dependencies
########################################

STACK ?= $(shell which stack)
NPM ?= $(shell which npm)
BROWSER_SYNC ?= $(shell which browser-sync)

.PHONY: setup
setup:
ifeq (,$(wildcard $(STACK)))
	@echo "Setup requires the Haskell Tool Stack"
	@echo "See: https://docs.haskellstack.org/en/stable/install_and_upgrade/"
endif
ifeq (,$(wildcard $(BROWSER_SYNC)))
ifeq (,$(wildcard $(NPM)))
	@echo "Seting up Browsersync requires the Node Package Manager"
	@echo "See: https://www.npmjs.com/get-npm"
else
	@echo "Installing Browsersync..."
	@$(NPM) install -g browser-sync
endif
endif

########################################
# Build blog with Hakyll
########################################

.PHONY: build
build:
	$(STACK) build && $(STACK) run build


########################################
# Start server
########################################

.PHONY: start
start: .watch.PID .serve.PID

.PHONY: stop
stop: watch-stop serve-stop

.PHONY: watch-start
watch-start: .watch.PID

.watch.PID:
	@echo "Building site.hs..."
	@$(STACK) build
	@echo "Starting Hakyll watch process..."
	@{ $(STACK) run watch -- --no-server 1>&2 & echo $$! > $@; }

.PHONY: watch-stop
watch-stop:
ifneq (,$(wildcard .watch.PID))
	kill `cat .watch.PID`
	rm .watch.PID
endif

.PHONY: serve-start
serve-start: .serve.PID

.serve.PID:
	@echo "Starting browser-sync..."
	@cd _site && { $(BROWSER_SYNC) start --server --files "." --no-ui  --reload-delay 500 --reload-debounce 500 1>&2 & echo $$! > $@; }

.PHONY: serve-stop
serve-stop:
ifneq (,$(wildcard .serve.PID))
	kill `cat .serve.PID`
	rm .serve.PID
endif


########################################
# Clean generated files
########################################

.PHONY: clean
clean:
	$(STACK) build && $(STACK) run clean


########################################
# Deploy blog
########################################

.PHONY: deploy
deploy:
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
		--filter='P CNAME'       \
		--delete-excluded        \
		_site/ .
	git add -A
	git commit -m "Publish."
	git push origin main:main
	git checkout dev
	git branch -D main

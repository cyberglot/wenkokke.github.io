OUT_DIR := _site
TMP_DIR := _cache

# OS = {Windows_NT, Linux, Darwin}
ifneq ($(OS),Windows_NT)
OS := $(shell uname -s)
endif


########################################
# Initialize Git Hooks
########################################

.PHONY: init
init:
	git config core.hooksPath .githooks

########################################
# Build site with Shake
########################################

CABAL ?= $(wildcard $(shell which cabal))

CABAL_ARGS += --verbose=0

.PHONY: build
build: check-haskell
	@$(CABAL) $(CABAL_ARGS) run builder -- build

.PHONY: clean
clean: check-haskell
	@$(CABAL) $(CABAL_ARGS) run builder -- clean

.PHONY: clobber
clobber: check-haskell
	@$(CABAL) $(CABAL_ARGS) run builder -- clobber


########################################
# Watch for changes with fswatch
########################################

FSWATCH ?= $(wildcard $(shell which fswatch))

FSWATCH_ARGS += --event=IsFile
FSWATCH_ARGS += --event=Created
FSWATCH_ARGS += --event=Updated
FSWATCH_ARGS += --event=Removed
FSWATCH_ARGS += --event=Renamed
FSWATCH_ARGS += --event=MovedFrom
FSWATCH_ARGS += --event=MovedTo
FSWATCH_ARGS += --one-per-batch
FSWATCH_ARGS += --recursive
FSWATCH_ARGS += --exclude="\.git/.*"
FSWATCH_ARGS += --exclude="dist-newstyle/.*"
FSWATCH_ARGS += --exclude="$(OUT_DIR)/.*"
FSWATCH_ARGS += --exclude="$(TMP_DIR)/.*"
FSWATCH_ARGS += "."

.PHONY: watch
watch:
	$(FSWATCH) $(FSWATCH_ARGS) | xargs -n1 -I{} make build


########################################
# Start local server with BrowserSync
########################################

NPX ?= $(wildcard $(shell which npx))
BROWSER_SYNC ?= $(wildcard $(shell which browser-sync))

BROWSER_SYNC_ARGS += start
BROWSER_SYNC_ARGS += --server
BROWSER_SYNC_ARGS += --files "."
BROWSER_SYNC_ARGS += --no-ui
BROWSER_SYNC_ARGS += --reload-delay 500
BROWSER_SYNC_ARGS += --reload-debounce 500

.PHONY: serve
serve: check-browser-sync
	@(cd $(OUT_DIR) && $(BROWSER_SYNC) $(BROWSER_SYNC_ARGS))


########################################
# Test site with HTMLProofer
########################################

RBENV ?= $(wildcard $(shell which rbenv))
HTML_PROOFER ?= $(wildcard $(shell which htmlproofer))

HTML_PROOFER_ARGS += --check-html
HTML_PROOFER_ARGS += --report-invalid-tags
HTML_PROOFER_ARGS += --report-missing-names
HTML_PROOFER_ARGS += --report-script-embeds
HTML_PROOFER_ARGS += --report-missing-doctype
HTML_PROOFER_ARGS += --report-eof-tags
HTML_PROOFER_ARGS += --report-mismatched-tags
HTML_PROOFER_ARGS += --check-img-http
HTML_PROOFER_ARGS += --check-opengraph
HTML_PROOFER_ARGS += .

.PHONY: test
test: build check-html-proofer
	@(cd $(OUT_DIR) && $(HTML_PROOFER) $(HTML_PROOFER_ARGS))


########################################
# Deploy blog
########################################

# .PHONY: deploy
# deploy:
# 	@echo "Cleaning up..."
# 	MODE=production make clean
# 	@echo "Building production site..."
# 	MODE=production make build
# 	@echo "Creating main branch..."
# 	git fetch --all
# 	git checkout -b main --track origin/main
# 	rsync -a \
# 		--filter='P _production/site/' \
# 		--filter='P _production/cache/' \
# 		--filter='P .git/' \
# 		--filter='P .gitignore' \
# 		--filter='P .stack-work' \
# 		--filter='P CNAME' \
# 		--delete-excluded \
# 		_production/site/ .
# 	git add -A
# 	@echo "Publishing main branch..."
# 	git commit -m "Publish."
# 	git push origin main:main
# 	@echo "Deleting main branch..."
# 	git checkout dev
# 	git branch -D main


########################################
# Dependencies for build
########################################

.PHONY: check-haskell
check-haskell:
ifeq (,$(CABAL))
	@echo "This task requires GHC and Cabal:"
	@echo "https://www.haskell.org/ghcup/"
	@exit 1
endif

########################################
# Dependencies for watch
########################################

.PHONY: check-fswatch
ifeq (,$(FSWATCH))
check-fswatch:
	@echo "This task requires fswatch:"
	@echo "https://github.com/emcrisostomo/fswatch#getting-fswatch"
	@exit 1
endif


########################################
# Dependencies for serve
########################################

.PHONY: check-browser-sync
ifeq (,$(BROWSER_SYNC))
check-browser-sync: check-node
	@$(eval BROWSER_SYNC := npx browser-sync)
endif

.PHONY: check-node
ifeq (,$(NPX))
check-node:
	@echo "This task requires Node.js:"
	@echo "https://nodejs.org/en/download/"
	@exit 1
endif


########################################
# Dependencies for test
########################################

.PHONY: check-html-proofer
ifeq (,$(HTML_PROOFER))
check-html-proofer: check-rbenv
	@$(RBENV) exec gem install --silent bundler
	@$(RBENV) exec bundle install --quiet
	@$(eval HTML_PROOFER := $(RBENV) exec bundle exec htmlproofer)
endif

.PHONY: check-rbenv
ifeq (,$(RBENV))
check-rbenv:
	@echo "This task requires RBEnv:"
	@echo "https://github.com/rbenv/rbenv#installation"
	@exit 1
endif

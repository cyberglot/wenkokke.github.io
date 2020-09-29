########################################
# Build mode (draft, production)
########################################

MODE ?= draft

ifeq (draft,$(MODE))
HAKYLL_FLAGS += --drafts
BUILD_DIR := $(abspath _draft)
else
BUILD_DIR := $(abspath _production)
endif

SITE_DIR := $(BUILD_DIR)/site
CACHE_DIR := $(BUILD_DIR)/cache
TMP_DIR := $(CACHE_DIR)/tmp
WATCH_PID := $(BUILD_DIR)/watch.pid
SERVER_PID := $(BUILD_DIR)/server.pid


########################################
# Build blog with Hakyll
########################################

.PHONY: build
build: $(SITE_DIR)

$(SITE_DIR): site.hs index.html bib csl css drafts pages posts public templates
	stack build && stack run build -- $(HAKYLL_FLAGS)

########################################
# Test blog with HTMLProofer
########################################

.PHONY: test
test: $(SITE_DIR)
	cd $(SITE_DIR) \
		&& htmlproofer \
			--check-html \
			--report-invalid-tags \
			--report-missing-names \
			--report-script-embeds \
			--report-missing-doctype \
			--report-eof-tags \
			--report-mismatched-tags \
			--check-img-http \
			--check-opengraph \
			.


########################################
# Start server
########################################

$(WATCH_PID):
	@echo "Building site..."
	@stack build && stack run build -- $(HAKYLL_FLAGS)
	@echo "Starting watch process..."
	@mkdir -p $(BUILD_DIR)
	@stack run watch -- $(HAKYLL_FLAGS) --no-server 1>&2 & echo $$! > $(WATCH_PID)

.PHONY: start-watch
start-watch: $(WATCH_PID)

.PHONY: stop-watch
stop-watch:
ifneq (,$(wildcard $(WATCH_PID)))
	kill `cat $(WATCH_PID)` && rm $(WATCH_PID)
endif

$(SERVER_PID): $(SITE_DIR)
	@echo "Starting server..."
	@cd $(SITE_DIR) && { \
		browser-sync start \
			--server \
			--files "." \
			--no-ui \
			--reload-delay 500 \
			--reload-debounce 500 \
			1>&2 & echo $$! > $(SERVER_PID); }

.PHONY: start-server
start-server: $(SERVER_PID)

.PHONY: stop-server
stop-server:
ifneq (,$(wildcard $(SERVER_PID)))
	kill `cat $(SERVER_PID)` && rm $(SERVER_PID)
endif

.PHONY: start
start:
	@make start-watch && make start-server

.PHONY: stop
stop:
	@make stop-watch && make stop-server


########################################
# Clean generated files
########################################

.PHONY: clean
clean:
	@stack build \
		&& stack run clean \
		&& stack run clean -- --drafts


########################################
# Deploy blog
########################################

.PHONY: deploy
deploy:
	@echo "Cleaning up..."
	MODE=production make clean
	@echo "Building production site..."
	MODE=production make build
	@echo "Creating main branch..."
	git fetch --all
	git checkout -b main --track origin/main
	rsync -a \
		--filter='P _production/site/' \
		--filter='P _production/cache/' \
		--filter='P .git/' \
		--filter='P .gitignore' \
		--filter='P .stack-work' \
		--filter='P CNAME' \
		--delete-excluded \
		_production/site/ .
	git add -A
	@echo "Publishing main branch..."
	git commit -m "Publish."
	git push origin main:main
	@echo "Deleting main branch..."
	git checkout dev
	git branch -D main


########################################
# Setup Git Hooks
########################################

.PHONY: init
init:
	git config core.hooksPath .githooks


########################################
# Setup dependencies
########################################

.PHONY: setup
setup: check-stack install-browser-sync install-html-proofer

.PHONY: check-stack
check-stack:
ifeq (,$(wildcard $(shell which stack)))
	@echo "Setup requires the Haskell Tool Stack"
	@echo "See: https://docs.haskellstack.org/en/stable/install_and_upgrade/"
	@exit 1
endif

.PHONY: check-npm
ifeq (,$(wildcard $(shell which npm)))
	@echo "Setup requires the Node Package Manager"
	@echo "See: https://www.npmjs.com/get-npm"
	@exit 1
endif

.PHONY: check-gem
check-gem:
ifeq (,$(wildcard $(shell which gem)))
	@echo "Setup requires the RubyGems Package Manager"
	@echo "See: https://www.ruby-lang.org/en/documentation/installation/"
	@exit 1
endif

.PHONY: install-browser-sync
install-browser-sync: check-npm
ifeq (,$(wildcard $(shell which browser-sync)))
	@echo "Installing Browsersync..."
	npm install -g browser-sync
endif

.PHONY: install-html-proofer
install-html-proofer: check-npm
ifeq (,$(wildcard $(shell which htmlproofer)))
	@echo "Installing HTMLProofer..."
	gem install html-proofer
endif

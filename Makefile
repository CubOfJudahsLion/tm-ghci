#! /usr/bin/make

# Utilties to deploy and package the plugin
# Requirements: pandoc, pdflatex, git, zip, tar, sed, cabal, ghc


# System values
MACHINE				:= $(shell uname -m | sed -re 's/^(.*)$$/\L\1/')
OS				:= $(shell uname -s | sed -re 's/^(.*)$$/\L\1/')
VERSION				:= $(shell git tag -l --sort="v:refname" "v*" | tail -n 1 | sed -re 's/^(.*)$$/\L\1/')
RELEASE_FILE_NO_EXT		:= tm-ghci.$(MACHINE).$(OS).$(VERSION)

# Static values
BASE_GHCI_NAME			:= GHCIInterface
PLUGIN_DIR			:= ghci
BIN_DIR				:= $(PLUGIN_DIR)/bin
DOC_DIR				:= $(PLUGIN_DIR)/doc
DOC_BASE_NAMES			:= ghci.en.tm ghci-abstract.en.tm ghci-demo.en.tm ghci-contact.en.tm haskell.png
DOC_FILES			:= $(foreach basename,$(DOC_BASE_NAMES), $(DOC_DIR)/$(basename))
SCHEME_DIR			:= $(PLUGIN_DIR)/progs
SCHEME_FILES			:= $(SCHEME_DIR)/init-ghci.scm
PLUGIN_SUBDIRS			:= $(BIN_DIR) $(DOC_DIR) $(SCHEME_DIR)

# OS-dependent values
ifneq ("$(USERPROFILE)","") # Windows variables
	PROFILE_PLUGINS_DIR	:= $(USERPROFILE)\\AppData\\Roaming\\TeXmacs\\plugins\\
	RELEASE_FILE		:= $(RELEASE_FILE_NO_EXT).zip
	PACK_CMD		:= zip -q9 $(RELEASE_FILE) $(PLUGIN_DIR)
	PLUGIN_SUBDIRS_DIRSEP	:= $(foreach subdir,$(PLUGIN_SUBDIRS), $(subst /,\\, $(source))
else # Un*x variables
	PROFILE_PLUGINS_DIR	:= $(HOME)/.TeXmacs/plugins/
	BIN_EXT			:= .bin
	RELEASE_FILE		:= $(RELEASE_FILE_NO_EXT).tar.xz
	PACK_CMD		:= tar -cJf $(RELEASE_FILE) $(PLUGIN_DIR)
	PLUGIN_SUBDIRS_DIRSEP	:= $(PLUGIN_SUBDIRS)
endif

GHCI_BINARY			:= $(BIN_DIR)/$(BASE_GHCI_NAME)$(BIN_EXT)
DEPLOY_SOURCES			:= $(DOC_FILES) $(SCHEME_FILES) $(GHCI_BINARY)

ifneq ("$(USERPROFILE)","")
	DEPLOY_SOURCES_DIRSEP	:= $(foreach source,$(DEPLOY_SOURCES), $(subst /,\\, $(source)))
else
	DEPLOY_SOURCES_DIRSEP	:= $(DEPLOY_SOURCES)
endif

DEPLOY_TARGETS			:= $(foreach source,$(DEPLOY_SOURCES_DIRSEP), $(PROFILE_PLUGINS_DIR)$(source))


.PHONY: help readme deploy release clean cleanall vartest

.ONESHELL:


help:
	@echo $$'\e[1;33mType:'
	@echo $$'\e[1;33m*\e[0m \e[36mmake readme\e[0m  to create the README files'
	@echo $$'\e[1;33m*\e[0m \e[36mmake release\e[0m to create a release archive'
	@echo $$'\e[1;33m*\e[0m \e[36mmake deploy\e[0m  to install the plugin to \e[34m$(PROFILE_PLUGINS_DIR)\e[0m'

$(GHCI_BINARY):
	@echo $$'\e[1;36m::\e[0m Creating directory $(BIN_DIR)'
	mkdir -p $(BIN_DIR)
	@echo $$'\e[1;36m::\e[0m Building'
	cabal build $(BASE_GHCI_NAME)
	@echo $$'\e[1;36m::\e[0m Copying to $(BIN_DIR)'
	cp $(shell cabal list-bin $(BASE_GHCI_NAME)) $@

readme:
	cd readme-src && $(MAKE)

$(DEPLOY_TARGETS): $(DEPLOY_SOURCES)
	@echo $$'\e[1;36m::\e[0m Creating target directories'
	@mkdir -p $(foreach subdir,$(PLUGIN_SUBDIRS_DIRSEP), $(PROFILE_PLUGINS_DIR)$(subdir))
	@echo $$'\e[1;36m::\e[0m Copying files'
	for source in $(DEPLOY_SOURCES_DIRSEP); do cp $$source $(PROFILE_PLUGINS_DIR)$$source; done

deploy: $(DEPLOY_TARGETS)

$(RELEASE_FILE): $(DEPLOY_SOURCES)
	@echo $$'\e[1;36m::\e[0m Creating compressed archive'
	$(PACK_CMD)

release: $(RELEASE_FILE)

clean:
	@echo $$'\e[1;36m::\e[0m Cleaning up temp files'
	@cabal clean
	@cd readme-src
	$(MAKE) clean

cleanall: clean
	@echo $$'\e[1;36m::\e[0m Cleaning up outputs'
	@rm -f ghci/bin/*
	@rm -f $(RELEASE_FILE_NO_EXT).*
	@cd readme-src
	$(MAKE) cleanall

vartest:
	@echo $$'\e[1;36m BASE_GHCI_NAME       :\e[0m' $(BASE_GHCI_NAME)
	@echo $$'\e[1;36m BIN_DIR              :\e[0m' $(BIN_DIR)
	@echo $$'\e[1;36m BIN_EXT              :\e[0m' $(BIN_EXT)
	@echo $$'\e[1;36m DEPLOY_SOURCES       :\e[0m' $(DEPLOY_SOURCES)
	@echo $$'\e[1;36m DEPLOY_SOURCES_DIRSEP:\e[0m' $(DEPLOY_SOURCES_DIRSEP)
	@echo $$'\e[1;36m DEPLOY_TARGETS       :\e[0m' $(DEPLOY_TARGETS)
	@echo $$'\e[1;36m DOC_BASE_NAMES       :\e[0m' $(DOC_BASE_NAMES)
	@echo $$'\e[1;36m DOC_DIR              :\e[0m' $(DOC_DIR)
	@echo $$'\e[1;36m DOC_FILES            :\e[0m' $(DOC_FILES)
	@echo $$'\e[1;36m GHCI_BINARY          :\e[0m' $(GHCI_BINARY)
	@echo $$'\e[1;36m MACHINE              :\e[0m' $(MACHINE)
	@echo $$'\e[1;36m OS                   :\e[0m' $(OS)
	@echo $$'\e[1;36m PACK_CMD             :\e[0m' $(PACK_CMD)
	@echo $$'\e[1;36m PLUGIN_DIR           :\e[0m' $(PLUGIN_DIR)
	@echo $$'\e[1;36m PLUGIN_SUBDIRS       :\e[0m' $(PLUGIN_SUBDIRS)
	@echo $$'\e[1;36m PLUGIN_SUBDIRS_DIRSEP:\e[0m' $(PLUGIN_SUBDIRS_DIRSEP)
	@echo $$'\e[1;36m PROFILE_PLUGINS_DIR  :\e[0m' $(PROFILE_PLUGINS_DIR)
	@echo $$'\e[1;36m RELEASE_FILE         :\e[0m' $(RELEASE_FILE)
	@echo $$'\e[1;36m RELEASE_FILE_NO_EXT  :\e[0m' $(RELEASE_FILE_NO_EXT)
	@echo $$'\e[1;36m SCHEME_DIR           :\e[0m' $(SCHEME_DIR)
	@echo $$'\e[1;36m SCHEME_FILES         :\e[0m' $(SCHEME_FILES)
	@echo $$'\e[1;36m VERSION              :\e[0m' $(VERSION)


# Scripts for building and other things.
# The script takes into account Win platforms,
# but you do need MSYS2 for it to run properly.


.PHONY: help compile clean cleanall deploy release vartest

.ONESHELL:

# Invariants
OWN_DIR			:= ghci
BIN_DIR			:= bin

# Variable by system
MACHINE			:= $(shell uname -m | sed -re 's/^(.*)$$/\L\1/')
OS			:= $(shell uname -s | sed -re 's/^(.*)$$/\L\1/')
VERSION			:= $(shell git tag -l "v*" | tail -n 1 | sed -re 's/^(.*)$$/\L\1/')
RELEASE_FILE_NO_EXT	:= tm-ghci.$(MACHINE).$(OS).$(VERSION)

# Un*x variables
EXE_EXT 		:= .bin
DIR_SEP_CHAR		:= /
TEXMACS_PLUGIN_DIR	:= ${HOME}/.TeXmacs/plugins
COPY_CMD		:= cp
MKDIR_CMD		:= mkdir -p
PIE			:= -fPIE
RELEASE_FILE		:= $(RELEASE_FILE_NO_EXT).tar.xz
PACK_CMD		:= tar cf $(RELEASE_FILE)

# Windows variables
ifneq ("$(USERPROFILE)","")
	DIR_SEP_CHAR		:= \\
	EXE_EXT			:= .exe
	SET_FILE		:= set_file.txt
	TEMP			:= $(shell set > $(SET_FILE))
	TEMP 			:= $(shell sed -r -i -e 's/\\/\\\\/g' $(SET_FILE))
	TEMP 			:= $(shell sed -r -n -e "/\\bUSERPROFILE\\b/{s/^[^=]+=//;s/'//g;p}" $(SET_FILE))
	TEXMACS_PLUGIN_DIR	:= $(TEMP)\\AppData\\Roaming\\TeXmacs\\plugins
	TEMP			:= $(shell rm $(SET_FILE))
	PIE			:=
	RELEASE_FILE		:= $(RELEASE_FILE_NO_EXT).zip
	PACK_CMD		:= zip -q9 $(RELEASE_FILE)
endif

# Configuration variables
BASE_NAME	:= GHCIInterface
SOURCE_FILE	:= src$(DIR_SEP_CHAR)$(BASE_NAME).hs
TARGET_EXE	:= $(BIN_DIR)$(DIR_SEP_CHAR)$(BASE_NAME)$(EXE_EXT)
DOC_FNAMES	:= ghci.en.tm ghci-abstract.en.tm ghci-demo.en.tm ghci-contact.en.tm haskell.png
DOC_FILES	:= $(foreach fname,$(DOC_FNAMES), doc$(DIR_SEP_CHAR)$(fname))
SCHEME_FILE	:= progs$(DIR_SEP_CHAR)init-ghci.scm

DEPLOY_DIR	:= $(TEXMACS_PLUGIN_DIR)$(DIR_SEP_CHAR)$(OWN_DIR)
DOC_DEPLOY_DIR	:= $(DEPLOY_DIR)$(DIR_SEP_CHAR)doc

DEPLOY_TARGET_EXE	:= $(DEPLOY_DIR)$(DIR_SEP_CHAR)$(TARGET_EXE)
DEPLOY_DOC_FILES	:= $(foreach fpath,$(DOC_FILES),$(DEPLOY_DIR)$(DIR_SEP_CHAR)$(fpath))
DEPLOY_SCHEME_FILE	:= $(DEPLOY_DIR)$(DIR_SEP_CHAR)$(SCHEME_FILE)


help:
	@echo $$'\e[1;33mType\e[0m:'
	@echo $$'\e[1;33m*\e[0m \e[1;36mmake clean\e[0m    to remove temp files'
	@echo $$'\e[1;33m*\e[0m \e[1;36mmake cleanall\e[0m to remove temp files, executables and release files'
	@echo $$'\e[1;33m*\e[0m \e[1;36mmake compile\e[0m  to create executable $(TARGET_EXE)'
	@echo $$'\e[1;33m*\e[0m \e[1;36mmake deploy\e[0m   to install the plugin locally'
	@echo $$'\e[1;33m*\e[0m \e[1;36mmake release\e[0m  to create the release file for your architecture, system and version'
	@echo $$'                (from the last "\e[1;34mv*\e[0m" \e[1mgit tag\e[0m): \e[1;34m$(MACHINE).$(OS).$(VERSION)\e[0m'

$(TARGET_EXE): $(SOURCE_FILE)
	@echo $$':: \e[1mEnsuring output directory exists\e[0m'
	$(MKDIR_CMD) $(BIN_DIR)
	@echo $$':: \e[1mCompiling\e[0m'
	ghc -O2 -g0 $(PIE) $< -o $@
	@echo $$':: \e[1mStripping\e[0m'
	strip -s -x -w -R .comment -R .note\* $@

compile: $(TARGET_EXE)

vartest:
	@echo $$'\e[1;36mCOPY_CMD          :\e[0m $(COPY_CMD)'
	@echo $$'\e[1;36mDEPLOY_DOC_FILES  :\e[0m $(DEPLOY_DOC_FILES)'
	@echo $$'\e[1;36mDEPLOY_SCHEME_FILE:\e[0m $(DEPLOY_SCHEME_FILE)'
	@echo $$'\e[1;36mDEPLOY_TARGET_EXE :\e[0m $(DEPLOY_TARGET_EXE)'
	@echo $$'\e[1;36mDIR_SEP_CHAR      :\e[0m $(DIR_SEP_CHAR)'
	@echo $$'\e[1;36mDOC_FILES         :\e[0m $(DOC_FILES)'
	@echo $$'\e[1;36mEXE_EXT           :\e[0m $(EXE_EXT)'
	@echo $$'\e[1;36mMACHINE           :\e[0m $(MACHINE)'
	@echo $$'\e[1;36mMKDIR_CMD         :\e[0m $(MKDIR_CMD)'
	@echo $$'\e[1;36mOS                :\e[0m $(OS)'
	@echo $$'\e[1;36mPACK_CMD          :\e[0m $(PACK_CMD)'
	@echo $$'\e[1;36mPIE               :\e[0m $(PIE)'
	@echo $$'\e[1;36mRELEASE_FILE      :\e[0m $(RELEASE_FILE)'
	@echo $$'\e[1;36mSCHEME_FILE       :\e[0m $(SCHEME_FILE)'
	@echo $$'\e[1;36mTARGET_EXE        :\e[0m $(TARGET_EXE)'
	@echo $$'\e[1;36mTEXMACS_PLUGIN_DIR:\e[0m $(TEXMACS_PLUGIN_DIR)'
	@echo $$'\e[1;36mUSERPROFILE       :\e[0m $(USERPROFILE)'
	@echo $$'\e[1;36mVERSION           :\e[0m $(VERSION)'

clean:
	@echo $$':: \e[1mCleaning residual files\e[0m'
	@if [[ -n "$(SET_FILE)" && -f "$(SET_FILE)" ]]; then rm "$(SET_FILE)"; fi
	@find src \( -iname \*.o -o -iname \*.hi -o -iname \*\~ \) -delete

cleanall: clean
	@echo $$':: \e[1mCleaning binaries and release files\e[0m'
	@rm -rf bin
	@find .. -name $(RELEASE_FILE) -delete

$(DEPLOY_TARGET_EXE) $(DEPLOY_DOC_FILES) $(DEPLOY_SCHEME_FILE): $(TARGET_EXE) $(DOC_FILES) $(SCHEME_FILE)
	@echo $$':: \e[1mDeploying in your local TeXmacs plugin directory\e[0m'
	@$(MKDIR_CMD) $(DEPLOY_DIR)$(DIR_SEP_CHAR){bin,doc,progs}
	@$(COPY_CMD) $(TARGET_EXE) $(DEPLOY_TARGET_EXE)
	@$(COPY_CMD) $(SCHEME_FILE) $(DEPLOY_SCHEME_FILE)
	@$(COPY_CMD) $(DOC_FILES) $(DOC_DEPLOY_DIR)

deploy: $(DEPLOY_TARGET_EXE) $(DEPLOY_DOC_FILE) $(DEPLOY_DOC_IMG_FILE) $(DEPLOY_SCHEME_FILE)

..$(DIR_SEP_CHAR)$(RELEASE_FILE): $(TARGET_EXE) $(DOC_FILES) $(SCHEME_FILE)
	@echo $$':: \e[1mCreating \e[34m..$(DIR_SEP_CHAR)$(RELEASE_FILE)\e[0m'
	@cd ..
	@-rm ../$(RELEASE_FILE) &> /dev/null
	$(PACK_CMD) $(foreach file,$^,ghci/$(file))

release: ..$(DIR_SEP_CHAR)$(RELEASE_FILE)


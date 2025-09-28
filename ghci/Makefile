# Scripts for building and other things.
# The script takes into account Win platforms,
# but you do need MSYS2 for it to run properly.


.PHONY: help compile clean cleanall deploy release vartest
.ONESHELL:

MACHINE			:= $(shell uname -m | sed -re 's/^(.*)$$/\L\1/')
OS			:= $(shell uname -s | sed -re 's/^(.*)$$/\L\1/')
VERSION			:= $(shell git tag -l "v*" | tail -n 1 | sed -re 's/^(.*)$$/\L\1/')
OWN_DIR			:= ghci
RELEASE_FILE_NO_EXT	:= tm-ghci.$(MACHINE).$(OS).$(VERSION)
BIN_DIR			:= bin

# Choose an extension appropiate to the OS
EXE_EXT 		:= .bin
DIR_SEP_CHAR		:= /
TEXMACS_PLUGIN_DIR	:= ${HOME}/.TeXmacs/plugins
COPY_CMD		:= cp
MKDIR_CMD		:= mkdir -p
PIE			:= -fPIE
RELEASE_FILE		:= $(RELEASE_FILE_NO_EXT).tar.xz
PACK_CMD		:= tar -cf $(RELEASE_FILE)

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
	@echo "Run: "
	@echo "- make clean    to remove temp files"
	@echo "- make cleanall to remove temp files, executables and release files"
	@echo "- make compile  to create executable $(TARGET_EXE)"
	@echo "- make deploy   to install the plugin locally"
	@echo "- make release  to create the release file for your architecture and version tag ($(MACHINE).$(OS).$(VERSION))"

$(TARGET_EXE): $(SOURCE_FILE)
	@echo :: Ensuring output directory exists
	$(MKDIR_CMD) $(BIN_DIR)
	@echo :: Compiling
	ghc -O2 -g0 $(PIE) $< -o $@
	@echo :: Stripping
	strip -s -x -w -R .comment -R .note\* $@

compile: $(TARGET_EXE)

vartest:
	@echo MACHINE		  = $(MACHINE)
	@echo OS	          = $(OS)
	@echo VERSION	          = $(VERSION)
	@echo USERPROFILE         = $(USERPROFILE)
	@echo EXE_EXT             = $(EXE_EXT)
	@echo DIR_SEP_CHAR        = $(DIR_SEP_CHAR)
	@echo TEXMACS_PLUGIN_DIR  = $(TEXMACS_PLUGIN_DIR)
	@echo COPY_CMD            = $(COPY_CMD)
	@echo MKDIR_CMD           = $(MKDIR_CMD)
	@echo PIE                 = $(PIE)
	@echo TARGET_EXE          = $(TARGET_EXE)
	@echo DOC_FILES           = $(DOC_FILES)
	@echo SCHEME_FILE         = $(SCHEME_FILE)
	@echo DEPLOY_TARGET_EXE   = $(DEPLOY_TARGET_EXE)
	@echo DEPLOY_DOC_FILES    = $(DEPLOY_DOC_FILES)
	@echo DEPLOY_SCHEME_FILE  = $(DEPLOY_SCHEME_FILE)
	@echo RELEASE_FILE        = $(RELEASE_FILE)
	@echo PACK_CMD            = $(PACK_CMD)

clean:
	if [[ -n "$(SET_FILE)" && -f "$(SET_FILE)" ]]; then rm "$(SET_FILE)"; fi
	find src \( -iname \*.o -o -iname \*.hi -o -iname \*\~ \) -delete

cleanall: clean
	rm -rf bin
	find .. -name $(RELEASE_FILE) -delete

$(DEPLOY_TARGET_EXE) $(DEPLOY_DOC_FILES) $(DEPLOY_SCHEME_FILE): $(TARGET_EXE) $(DOC_FILES) $(SCHEME_FILE)
	@echo :: Deploying in your local TeXmacs plugin directory
	$(MKDIR_CMD) $(DEPLOY_DIR)$(DIR_SEP_CHAR){bin,doc,progs}
	@$(COPY_CMD) $(TARGET_EXE) $(DEPLOY_TARGET_EXE)
	@$(COPY_CMD) $(SCHEME_FILE) $(DEPLOY_SCHEME_FILE)
	$(COPY_CMD) $(DOC_FILES) $(DOC_DEPLOY_DIR)

deploy: $(DEPLOY_TARGET_EXE) $(DEPLOY_DOC_FILE) $(DEPLOY_DOC_IMG_FILE) $(DEPLOY_SCHEME_FILE)

..$(DIR_SEP_CHAR)$(RELEASE_FILE): $(TARGET_EXE) $(DOC_FILES) $(SCHEME_FILE)
	@echo :: Creating ..$(DIR_SEP_CHAR)$(RELEASE_FILE)
	@cd ..
	@-rm ../$(RELEASE_FILE) &> /dev/null
	$(PACK_CMD) $(foreach file,$^,ghci/$(file))

release: ..$(DIR_SEP_CHAR)$(RELEASE_FILE)


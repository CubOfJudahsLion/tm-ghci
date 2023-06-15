# Scripts for building and other things.
# The script takes into account Widowsn platforms,
# but you do need MSYS2 for it to run properly.


.PHONY: help clean cleanall deploy release vartest


EXE_EXT 		:= .bin
TEXMACS_PLUGIN_DIR	:= ${HOME}/.TeXmacs/plugins
COPY_CMD		:= cp -f
MKDIR_CMD		:= mkdir -p
DYNAMIC_LIBS		:= -dynamic
PIE			:= -fPIE
PLATFORM		:= linux
VERSION_FILE		:= version.txt
TEMP 			:= $(shell git tag -l 'v-*' --sort=-version:refname \
				--format=%\(refname:strip=2\)_%\(objectname:short=8\) \
				--color=never > $(VERSION_FILE))
TEMP			:= $(shell sed -rne '/1/s/^v-/v/p' -i $(VERSION_FILE))
VERSION			:= $(shell cat $(VERSION_FILE))
RELEASE_EXT		:= tar.xz
RELEASE_FILE		:= tm-ghci.x86_64.$(PLATFORM).$(VERSION).tar.xz
PACK_CMD_NAME		:= tar
PACK_ARGS		:= -cJf
USER_PROFILE_FILE	:= profile.txt

# MSYS: Windows variables would be present
ifneq ("$(USERPROFILE)","")
	EXE_EXT			:= .exe
	TEMP			:= $(shell set > $(USER_PROFILE_FILE))
	TEMP 			:= $(shell sed -re 's/\\/\\\\/g' -i $(USER_PROFILE_FILE))
	TEMP 			:= $(shell sed -rne "/\\bUSERPROFILE\\b/{s/^[^=]+=//;s/'//g;p}" $(USER_PROFILE_FILE))
	TEXMACS_PLUGIN_DIR	:= $(TEMP)/AppData/Roaming/TeXmacs/plugins
	TEMP			:= $(shell rm $(USER_PROFILE_FILE))
	DYNAMIC_LIBS		:=
	PIE			:=
	PLATFORM		:= win32
	RELEASE_EXT 		:= zip
	PACK_CMD_NAME		:= zip
	PACK_ARGS		:= -q9
endif

RELEASE_FILE		:= tm-ghci.x86_64.$(PLATFORM).$(VERSION).$(RELEASE_EXT)
PACK_CMD		:= $(PACK_CMD_NAME) $(PACK_ARGS) $(RELEASE_FILE)

# Generate a version file we can use in Haskell
HS_VERSION_FILE		:= src/GitVersion.hs
TEMP			:= $(shell echo "module GitVersion where" > $(HS_VERSION_FILE))
TEMP			:= $(shell echo "gitVersion :: String" >> $(HS_VERSION_FILE))
TEMP			:= $(shell echo "gitVersion = \"$(VERSION)\"" >> $(HS_VERSION_FILE))
undefine TEMP

DOC_FNAMES		:= ghci.en.tm ghci-abstract.en.tm ghci-demo.en.tm ghci-contact.en.tm haskell.png
DOC_FILES		:= $(foreach fname,$(DOC_FNAMES), doc/$(fname))

SCHEME_FILE		:= progs/init-ghci.scm

DEPLOY_DIR		:= $(TEXMACS_PLUGIN_DIR)/ghci
DOC_DEPLOY_DIR		:= $(DEPLOY_DIR)/doc

DEPLOY_TARGET_EXE	:= $(DEPLOY_DIR)/$(TARGET_EXE)
DEPLOY_DOC_FILES	:= $(foreach fpath,$(DOC_FILES),$(DEPLOY_DIR)/$(fpath))
DEPLOY_SCHEME_FILE	:= $(DEPLOY_DIR)/$(SCHEME_FILE)

$(TARGET_EXE): tm-ghci.cabal src/Main.hs
	@echo :: Compiling session interface
	cabal build -j --bindir=bin -O2 --disable-debug-info --enable-executable-stripping
	#ghc -O2 -g0 $(DYNAMIC_LIBS) $(PIE) $< -o $@
	@echo :: Stripping
	strip -s -x -w -R .comment -R .note\* $@

.ONESHELL:
help: @echo "Calling make without arguments just builds"
	@echo "the executable $(TARGET_EXE)."
	@echo
	@echo "Call with 'clean'    to remove temp files"
	@echo "Call with 'cleanall' to remove temp files and executables"
	@echo "Call with 'deploy'   to install in your TeXmacs plugin directory"
	@echo "Call with 'release'  to create a release archive for the"
	@echo "                     current OS (in the parent directory.)"

vartest:
#	@echo USERPROFILE         = $(USERPROFILE)
	@echo EXE_EXT             = $(EXE_EXT)
	@echo TEXMACS_PLUGIN_DIR  = $(TEXMACS_PLUGIN_DIR)
	@echo COPY_CMD            = $(COPY_CMD)
	@echo MKDIR_CMD           = $(MKDIR_CMD)
	@echo DYNAMIC_LIBS        = $(DYNAMIC_LIBS)
	@echo PIE                 = $(PIE)
	@echo TARGET_EXE          = $(TARGET_EXE)
	@echo DOC_FILES           = $(DOC_FILES)
	@echo SCHEME_FILE         = $(SCHEME_FILE)
	@echo DEPLOY_TARGET_EXE   = $(DEPLOY_TARGET_EXE)
	@echo DEPLOY_DOC_FILES    = $(DEPLOY_DOC_FILES)
	@echo DEPLOY_SCHEME_FILE  = $(DEPLOY_SCHEME_FILE)
	@echo VERSION_FIlE        = $(VERSION_FILE)
	@echo VERSION             = $(VERSION)
	@echo USER_PROFILE_FILE   = $(USER_PROFILE_FILE)
	@echo HS_VERSION_FILE     = $(HS_VERSION_FILE)
	@echo PLATFORM            = $(PLATFORM)
	@echo RELEASE_FILE        = $(RELEASE_FILE)
	@echo PACK_CMD            = $(PACK_CMD)

clean:
	@-find . \( -name \*\~ -o -name \#\* -o -name .\*.sw\? -o -name \*.xz -o -name \*.zip \) -delete
	@-find . \( -name $(VERSION_FILE) -o -name $(USER_PROFILE_FILE) -o -name $(HS_VERSION_FILE) \) -delete

cleanall: clean
	@-find src \( -name \*.hi -o -name \*.o \) -delete
	@-rm -f bin/*

$(DEPLOY_TARGET_EXE): $(TARGET_EXE)
	@echo :: Copying executable
	@$(MKDIR_CMD) $(DEPLOY_DIR)/bin
	@$(COPY_CMD) $(TARGET_EXE) $(DEPLOY_TARGET_EXE)

$(DEPLOY_DOC_FILES): $(DOC_FILES)
	@echo :: Copying documentation
	@$(MKDIR_CMD) $(DEPLOY_DIR)/doc
	@$(COPY_CMD) $(DOC_FILES) $(DOC_DEPLOY_DIR)

$(DEPLOY_SCHEME_FILE): $(SCHEME_FILE)
	@echo :: Copying Scheme programs
	@$(MKDIR_CMD) $(DEPLOY_DIR)/progs
	@$(COPY_CMD) $(SCHEME_FILE) $(DEPLOY_SCHEME_FILE)

deploy: $(DEPLOY_TARGET_EXE) $(DEPLOY_DOC_FILES) $(DEPLOY_SCHEME_FILE)
	@echo :: Deployment finished

$(RELEASE_FILE): $(TARGET_EXE) $(DOC_FILES) $(SCHEME_FILE)
	@-rm -f $(RELEASE_FILE) &> /dev/null
	@$(PACK_CMD) $(foreach file,$^,ghci/$(file))

release: $(RELEASE_FILE)


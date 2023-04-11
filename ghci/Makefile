# Scripts for building and other things.
# The script takes into account Widowsn platforms,
# but you do need MSYS2 for it to run properly.


.PHONY: help clean cleanall deploy release vartest


# Choose an extension appropiate to the OS
OWN_DIR			:= ghci
EXE_EXT 		:= .bin
DIR_SEP_CHAR		:= /
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
RELEASE_FILE		:= tm-ghci.x86_64.$(PLATFORM).$(VERSION).tar.xz
PACK_CMD		:= tar -cJf $(RELEASE_FILE)
USER_PROFILE_FILE	:= profile.txt

# Windows variables
ifneq ("$(USERPROFILE)","")
	DIR_SEP_CHAR		:= \\
	EXE_EXT			:= .exe
	TEMP			:= $(shell set > $(USER_PROFILE_FILE))
	TEMP 			:= $(shell sed -re 's/\\/\\\\/g' -i $(USER_PROFILE_FILE))
	TEMP 			:= $(shell sed -rne "/\\bUSERPROFILE\\b/{s/^[^=]+=//;s/'//g;p}" $(USER_PROFILE_FILE))
	TEXMACS_PLUGIN_DIR	:= $(TEMP)\\AppData\\Roaming\\TeXmacs\\plugins
	TEMP			:= $(shell rm $(USER_PROFILE_FILE))
	DYNAMIC_LIBS		:=
	PIE			:=
	PLATFORM		:= win32
	RELEASE_FILE		:= tm-ghci.x86_64.$(PLATFORM).$(VERSION).zip
	PACK_CMD		:= zip -q9 $(RELEASE_FILE)
endif

# Generate a version file we can use in Haskell
HS_VERSION_FILE		:= src$(DIR_SEP_CHAR)GitVersion.hs
TEMP			:= $(shell echo "module GitVersion where" > $(HS_VERSION_FILE))
TEMP			:= $(shell echo "gitVersion :: String" >> $(HS_VERSION_FILE))
TEMP			:= $(shell echo "gitVersion = \"$(VERSION)\"" >> $(HS_VERSION_FILE))
undefine TEMP

RELEASE_FILE		:= ..$(DIR_SEP_CHAR)$(RELEASE_FILE)

BASE_NAME		:= GHCiInterface
SOURCE_FILE		:= src$(DIR_SEP_CHAR)$(BASE_NAME).hs
TARGET_EXE		:= bin$(DIR_SEP_CHAR)$(BASE_NAME)$(EXE_EXT)

DOC_FNAMES		:= ghci.en.tm ghci-abstract.en.tm ghci-demo.en.tm ghci-contact.en.tm haskell.png
DOC_FILES		:= $(foreach fname,$(DOC_FNAMES), doc$(DIR_SEP_CHAR)$(fname))

SCHEME_FILE		:= progs$(DIR_SEP_CHAR)init-ghci.scm

DEPLOY_DIR		:= $(TEXMACS_PLUGIN_DIR)$(DIR_SEP_CHAR)$(OWN_DIR)
DOC_DEPLOY_DIR		:= $(DEPLOY_DIR)$(DIR_SEP_CHAR)doc

DEPLOY_TARGET_EXE	:= $(DEPLOY_DIR)$(DIR_SEP_CHAR)$(TARGET_EXE)
DEPLOY_DOC_FILES	:= $(foreach fpath,$(DOC_FILES),$(DEPLOY_DIR)$(DIR_SEP_CHAR)$(fpath))
DEPLOY_SCHEME_FILE	:= $(DEPLOY_DIR)$(DIR_SEP_CHAR)$(SCHEME_FILE)

$(TARGET_EXE): $(SOURCE_FILE)
	@echo :: Compiling session interface $(SOURCE_FILE)
	ghc -O2 -g0 $(DYNAMIC_LIBS) $(PIE) $< -o $@
	@echo :: Stripping
	strip -s -x -w -R .comment -R .note\* $@

$(VERSION_FILE):


.ONESHELL:
help:
	@echo "Calling make without arguments just builds"
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
	@echo DIR_SEP_CHAR        = $(DIR_SEP_CHAR)
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
	@-find .. \( -name \*\~ -o -name \#\* -o -name .\*.sw\? -o -name \*.xz -o -name \*.zip \) -delete
	@-find src \( -name \*.hi -o -name \*.o \) -delete
	@-find . \( -name $(VERSION_FILE) -o -name $(USER_PROFILE_FILE) \) -delete

cleanall: clean
	@-find bin/ -name \* -delete && mkdir -p bin

$(DEPLOY_TARGET_EXE): $(TARGET_EXE)
	@echo :: Copying executable
	@$(MKDIR_CMD) $(DEPLOY_DIR)$(DIR_SEP_CHAR)bin
	@$(COPY_CMD) $(TARGET_EXE) $(DEPLOY_TARGET_EXE)

$(DEPLOY_DOC_FILES): $(DOC_FILES)
	@echo :: Copying documentation
	@$(MKDIR_CMD) $(DEPLOY_DIR)$(DIR_SEP_CHAR)doc
	@$(COPY_CMD) $(DOC_FILES) $(DOC_DEPLOY_DIR)

$(DEPLOY_SCHEME_FILE): $(SCHEME_FILE)
	@echo :: Copying Scheme programs
	@$(MKDIR_CMD) $(DEPLOY_DIR)$(DIR_SEP_CHAR)progs
	@$(COPY_CMD) $(SCHEME_FILE) $(DEPLOY_SCHEME_FILE)

deploy: $(DEPLOY_TARGET_EXE) $(DEPLOY_DOC_FILES) $(DEPLOY_SCHEME_FILE)
	@echo :: Deployment finished

.ONESHELL:
$(RELEASE_FILE): $(TARGET_EXE) $(DOC_FILES) $(SCHEME_FILE)
	@-rm -f $(RELEASE_FILE) &> /dev/null
	@cd ..
	@$(PACK_CMD) $(foreach file,$^,ghci/$(file))
	@cd ghci

release: $(RELEASE_FILE)


# Scripts for building and other things.
# This builds on Linux and MSys2 (the executables
# don't have MSys dependencies, though.)


.PHONY: help clean cleanall deploy release vartest

PROJECT_NAME		:= tm-ghci

DIR_SEP			:= /
EXE_EXT 		:=
TEXMACS_PLUGIN_DIR	:= ${HOME}/.TeXmacs/plugins
COPY_CMD		:= cp -f
MKDIR_CMD		:= mkdir -p
PLATFORM		:= $(subst /,-,$(shell uname -o | tr \[A-Z\] \[a-z\]))
MACHINE			:= $(shell uname -m)
VERSION			:= $(shell   git tag -l 'v-*' \
			                     --sort=-version:refname \
			                     --format=%\(refname:strip=2\)_%\(objectname:short=8\) \
					     --color=never \
			           | sed -re '/1/{ s/^v-/v/; p }; d')
RELEASE_EXT		:= tar.xz
PACK_CMD_NAME		:= tar
PACK_ARGS		:= -cJf

# MSys: Windows variables would be present
ifneq ("$(USERPROFILE)","")
	DIR_SEP			:= \\
	EXE_EXT			:= .exe
	USER_PROFILE		:= $(shell   set \
				           | sed -re "/^USERPROFILE=/ { \
				                        s/^USERPROFILE='([^']*)'/\1/; \
				                        s:\\\\:\\\\\\\\:gp; \
						      }; \
				                      d")
	TEXMACS_PLUGIN_DIR	:= $(USER_PROFILE)\\AppData\\Roaming\\TeXmacs\\plugins
	PLATFORM		:= win32
	RELEASE_EXT 		:= zip
	PACK_CMD_NAME		:= zip
	PACK_ARGS		:= -q9
endif

SOURCE_FILES		:= $(shell   ls -1R src \
			           | sed -re '/:$$/{ s|:$$|/|; h; d; }; \
				              /OlderSingle/ d; \
					      /hs2tm/ d; \
					      /\.hs/ { G; s/^([^\n]*)\n(.*)$$/\2\1/p; }; \
					      d')

# Generate a version file we can use in Haskell
HS_VERSION_FILENAME	:= GitVersion.hs
HS_VERSION_FILE 	:= src$(DIR_SEP)$(HS_VERSION_FILENAME)
TEMP			:= $(shell echo "module GitVersion where" > $(HS_VERSION_FILE))
TEMP			:= $(shell echo "gitVersion :: String" >> $(HS_VERSION_FILE))
TEMP			:= $(shell echo "gitVersion = \"$(VERSION)\"" >> $(HS_VERSION_FILE))
undefine TEMP

DOC_FILES		:= $(subst /,$(DIR_SEP),$(wildcard doc/$(PROJECT_NAME)-*.tm))

SCHEME_FILE		:= progs$(DIR_SEP)init-$(PROJECT_NAME).scm

DEPLOY_DIR		:= $(TEXMACS_PLUGIN_DIR)$(DIR_SEP)ghci
DOC_DEPLOY_DIR		:= $(DEPLOY_DIR)$(DIR_SEP)doc

CABAL_FILENAME		:= $(PROJECT_NAME).cabal
EXE_FILENAME		:= $(PROJECT_NAME)$(EXE_EXT)

TARGET_EXE		:= bin$(DIR_SEP)$(EXE_FILENAME)

DEPLOY_TARGET_EXE	:= $(DEPLOY_DIR)$(DIR_SEP)$(TARGET_EXE)
DEPLOY_DOC_FILES	:= $(foreach fpath,$(DOC_FILES),$(DEPLOY_DIR)$(DIR_SEP)$(fpath))
DEPLOY_SCHEME_FILE	:= $(DEPLOY_DIR)$(DIR_SEP)$(SCHEME_FILE)

RELEASE_FILE		:= $(PROJECT_NAME).$(MACHINE).$(PLATFORM).$(VERSION).$(RELEASE_EXT)
PACK_CMD		:= $(PACK_CMD_NAME) $(PACK_ARGS) $(RELEASE_FILE)


# ======= RULES =======

# Ignore failure in clean-up commands
.IGNORE: clean cleanall

# Most targets are not meant to eacho their recipes
.SILENT: $(TARGET_EXE) help vartest clean cleanall $(DEPLOY_TARGET_EXE) $(DEPLOY_DOC_FILES) $(DEPLOY_SCHEME_FILE) $(RELEASE_FILE)


$(TARGET_EXE): tm-ghci.cabal $(SOURCE_FILES)
	echo :: Compiling session interface
	cabal build -j --bindir=./bin --enable-executable-stripping
	$(MKDIR_CMD) bin
	find *dist* -name $(EXE_FILENAME) -executable -type f -exec $(COPY_CMD) {} $(TARGET_EXE) \;

.ONESHELL:
help:
	echo "Calling make without arguments just builds"
	echo "the release executable $(TARGET_EXE)."
	echo
	echo "Call with 'debug'    to build an executable for debugging,"
	echo "Call with 'clean'    to remove temp files,"
	echo "Call with 'cleanall' to remove temp files and executables,"
	echo "Call with 'deploy'   to install in your TeXmacs plugin directory,"
	echo "Call with 'release'  to create a release archive for the"
	echo "                     current OS (in the parent directory.)"

.ONESHELL:
vartest:
	echo CABAL_FILENAME      = $(CABAL_FILENAME)
	echo COPY_CMD            = $(COPY_CMD)
	echo DEPLOY_DOC_FILES    = $(DEPLOY_DOC_FILES)
	echo DEPLOY_SCHEME_FILE  = $(DEPLOY_SCHEME_FILE)
	echo DEPLOY_TARGET_EXE   = $(DEPLOY_TARGET_EXE)
	echo DIR_SEP             = $(DIR_SEP)
	echo DOC_FILES           = $(DOC_FILES)
	echo EXE_EXT             = $(EXE_EXT)
	echo EXE_FILENAME        = $(EXE_FILENAME)
	echo HS_VERSION_FILE     = $(HS_VERSION_FILE)
	echo MKDIR_CMD           = $(MKDIR_CMD)
	echo PACK_CMD            = $(PACK_CMD)
	echo PLATFORM            = $(PLATFORM)
	echo PLATFORM            = $(PLATFORM)
	echo PROJECT_NAME        = $(PROJECT_NAME)
	echo RELEASE_FILE        = $(RELEASE_FILE)
	echo SCHEME_FILE         = $(SCHEME_FILE)
	echo SCHEME_FILE         = $(SCHEME_FILE)
	echo SOURCE_FILES        = $(SOURCE_FILES)
	echo TARGET_EXE          = $(TARGET_EXE)
	echo TEXMACS_PLUGIN_DIR  = $(TEXMACS_PLUGIN_DIR)
	echo USERPROFILE         = $(USERPROFILE)
	echo VERSION             = $(VERSION)

clean:
	find . \( -name \*\~ -o -name \#\* -o -name .\*\.sw\? -o -name \*\.xz -o -name \*\.zip \) -delete
	find . -name $(HS_VERSION_FILENAME) -delete

cleanall: clean
	find src \( -name \*.hi -o -name \*.o \) -delete
	rm -f bin/*
	rm -rf dist-newstyle

$(DEPLOY_TARGET_EXE): $(TARGET_EXE)
	echo :: Copying executable
	$(MKDIR_CMD) $(DEPLOY_DIR)/bin
	$(COPY_CMD) $(TARGET_EXE) $(DEPLOY_TARGET_EXE)

$(DEPLOY_DOC_FILES): $(DOC_FILES)
	echo :: Copying documentation
	$(MKDIR_CMD) $(DEPLOY_DIR)/doc
	$(COPY_CMD) $? $(DOC_DEPLOY_DIR)

$(DEPLOY_SCHEME_FILE): $(SCHEME_FILE)
	echo :: Copying Scheme programs
	$(MKDIR_CMD) $(DEPLOY_DIR)/progs
	$(COPY_CMD) $(SCHEME_FILE) $(DEPLOY_SCHEME_FILE)

deploy: $(DEPLOY_TARGET_EXE) $(DEPLOY_DOC_FILES) $(DEPLOY_SCHEME_FILE)
	@echo :: Deployment finished

$(RELEASE_FILE): $(TARGET_EXE) $(DOC_FILES) $(SCHEME_FILE)
	rm -f $(RELEASE_FILE) &> /dev/null
	$(PACK_CMD) $^

release: $(RELEASE_FILE)


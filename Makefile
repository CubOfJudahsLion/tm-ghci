# Scripts for building, packing release files and installing the plugin.
# The included scripts work for Linux, FreeBSD and MSYS2 (Windows.)
#   In FreeBSD, use 'gmake' instead of 'make' (which is actually pmake.)
# Requirements: ghc 9.10.3+, git, zip/xz (if on Windows/Un*x respectively),
#   strip and some standard utilities (awk, sed, tar, printf.)


.ONESHELL:

.PHONY: help compile plug release vartest clean nuke unplug 


## Platform, version and derived values
# These are required for tagging the release archive
ARCH			:= $(shell uname -m | awk '{print tolower($$0)}')
OS			:= $(shell uname -so | sed -re 's/ /\n/g' | awk '/^[A-Za-z]+$$/{print tolower($$0)}')
VERSION			:= $(shell git tag -l "v*" | tail -n 1 | awk '{print tolower($$0)}')

# MSys is required to build, but it's not required to run a Windows executable
# so we give it the proper OS name to avoid confusions
ifeq ("$(OS)","msys")
	OS := win32
endif

# Release file without extension -- compression format preferences vary across platforms
RELEASE_FILE_NO_EXT	:= tm-ghci.$(ARCH).$(OS).$(VERSION)


## Name for both plugin distribution directory and target plugin subdirectory
PLUGIN_DIR		:= ghci


## Choose some values appropiate for the OS. POSIX defaults:
EXE_EXT 		:= .bin
TEXMACS_PLUGIN_DIR	:= $(HOME)/.TeXmacs/plugins
PIE			:= -fPIE
RELEASE_FILE		:= $(RELEASE_FILE_NO_EXT).tar.xz
PACK_CMD		:= tar -cJf $(RELEASE_FILE) $(PLUGIN_DIR)

# Windows:
ifeq ("$(OS)","win32")
	EXE_EXT			:= .exe
	TEXMACS_PLUGIN_DIR	:= $(HOME)/AppData/Roaming/TeXmacs/plugins
	PIE			:=	# Windows has no support for position-independent code
	RELEASE_FILE		:= $(RELEASE_FILE_NO_EXT).zip
	PACK_CMD		:= zip -qr9 $(RELEASE_FILE) $(PLUGIN_DIR)
endif


## Distribution files

# Variables for compilation and executable deployment
BASE_NAME		:= GHCIInterface
PROJECT_DIR		:= ghci-interface
SOURCE_SUBDIR		:= src
SOURCE_DIR		:= $(PROJECT_DIR)/$(SOURCE_SUBDIR)
SOURCE_FILES		:= $(shell find $(PROJECT_DIR) -iname \*.hs -o \
			                               -path $(PROJECT_DIR)/$(BASE_NAME).cabal -o \
			                               -regex '$(PROJECT_DIR)/\(package\|stack\)\.yaml')
EXE_NAME		:= $(BASE_NAME)$(EXE_EXT)
EXE_SUBDIR		:= bin
EXE_DIR			:= $(PLUGIN_DIR)/$(EXE_SUBDIR)
EXE_FILE		:= $(EXE_DIR)/$(EXE_NAME)

# Documentation variables
DOC_SUBDIR		:= doc
DOC_DIR			:= $(PLUGIN_DIR)/$(DOC_SUBDIR)
DOC_FILES		:= $(shell find $(DOC_DIR) \( -iname *.tm -o -iname *.png \))

# Scheme scripts
SCHEME_SUBDIR		:= progs
SCHEME_DIR		:= $(PLUGIN_DIR)/$(SCHEME_SUBDIR)
SCHEME_FILE		:= $(SCHEME_DIR)/init-ghci.scm

# All plugin distributable files
DIST_FILES		:= $(EXE_FILE) $(DOC_FILES) $(SCHEME_FILE)

# Counterparts to DIST_FILES, only used as targets further down
TARGET_FILES		:= $(foreach dist_file,$(DIST_FILES),$(TEXMACS_PLUGIN_DIR)/$(dist_file))


## Some ANSI code helpers

# Codes
ANSI_START		:= \x1B[
ANSI_BOLD		:= $(ANSI_START)1m
ANSI_RESET		:= $(ANSI_START)0m
ANSI_YELLOW		:= $(ANSI_START)33m
ANSI_CYAN		:= $(ANSI_START)36m
ANSI_BLUE		:= $(ANSI_START)34m

# Prints justified help statements (uses ANSI codes but resets at the end of every line.)
define help-line =
  @printf "  - $(ANSI_YELLOW)$(MAKE) $(ANSI_BOLD)%-8s $(ANSI_RESET)to %s$(ANSI_RESET)\n" "$(1)" "$(2)"
endef

# Dumps a variable formatted as "name = value" (justified, ANSI, resets.)
define dump-var =
  @printf "$(ANSI_YELLOW)%-20s$(ANSI_RESET) = $(ANSI_CYAN)%s$(ANSI_RESET)\n" "$(1)" "$($(1))"
endef

# Print a header action (justified, ANSI, resets.)
define action-header =
  @printf "$(ANSI_BOLD)$(ANSI_YELLOW):: $(ANSI_BLUE)%s$(ANSI_RESET)\n" "$(1)"
endef


## First (default) target just shows help
help:
	@echo Run:
	$(call help-line,compile,create executable $(ANSI_CYAN)$(EXE_FILE)$(ANSI_RESET) -- implied by $(ANSI_YELLOW)deploy$(ANSI_RESET) and $(ANSI_YELLOW)release)
	$(call help-line,plug   ,install the plugin locally (in $(ANSI_CYAN)$(TEXMACS_PLUGIN_DIR)$(ANSI_RESET)))
	$(call help-line,unplug ,uninstall the plugin)
	$(call help-line,release,create the release file for this platform/version ($(ANSI_BOLD)$(ANSI_BLUE)$(ARCH).$(OS).$(VERSION)$(ANSI_RESET)))
	$(call help-line,clean  ,remove temp files)
	$(call help-line,nuke   ,remove temp and target files and uninstall the plugin)
	$(call help-line,help   ,display this text)


## Building, packing and installing

# Compilation
$(EXE_FILE): $(SOURCE_FILES)
	$(call action-header,Ensuring output directory exists)
	mkdir -p $(dir $@)
	$(call action-header,Compiling)
	pushd $(SOURCE_DIR)
	# This compilation method is good enough for now, we'll set up building through cabal, stack and nix soon enough
	ghc -O2 -g0 -j -fdefer-diagnostics -package=process -package=deepseq Main -o $(EXE_NAME)
	popd
	mv $(SOURCE_DIR)/$(EXE_NAME) $@
	$(call action-header,Stripping)
	strip $@

compile: $(EXE_FILE)

# Plugin installation
$(TARGET_FILES): $(DIST_FILES)
	$(call action-header,Deploying in your local TeXmacs plugin directory)
	mkdir -p $(TEXMACS_PLUGIN_DIR)
	cp -r $(PLUGIN_DIR) $(TEXMACS_PLUGIN_DIR)

plug: $(TARGET_FILES)

# Release making
$(RELEASE_FILE): $(DIST_FILES)
	$(call action-herder,Creating release file $(RELEASE_FILE))
	@-rm -f $(RELEASE_FILE) &> /dev/null
	$(PACK_CMD)

release: $(RELEASE_FILE)


# Debuggin
# This dumps important vars for debuggin
vartest:
	$(call dump-var,ARCH)
	$(call dump-var,DIST_FILES)
	$(call dump-var,DOC_FILES)
	$(call dump-var,EXE_FILE)
	$(call dump-var,MAKE)
	$(call dump-var,OS)
	$(call dump-var,PACK_CMD)
	$(call dump-var,PIE)
	$(call dump-var,PLUGIN_DIR)
	$(call dump-var,PROJECT_DIR)
	$(call dump-var,RELEASE_FILE)
	$(call dump-var,SCHEME_FILE)
	$(call dump-var,SOURCE_DIR)
	$(call dump-var,SOURCE_FILES)
	$(call dump-var,TARGET_FILES)
	$(call dump-var,TEXMACS_PLUGIN_DIR)
	$(call dump-var,VERSION)


## Cleaning
# Clean temporary compilation files
clean:
	$(call action-header,Deleting temp files)
	@-find $(PROJECT_DIR) \( -iname \*.o -o -iname \*.hi -o -iname \*\~ \) -delete 2> /dev/null

# Uninstall the plugin
unplug:
	$(call action-header,Uninstalling local plugin)
	@-rm -rf $(TEXMACS_PLUGIN_DIR)/$(PLUGIN_DIR) 2> /dev/null

# Clean temporary, target and installed plugin files
nuke: clean unplug
	$(call action-header,Removing binary and release file)
	@-rm -rf $(EXE_DIR) 2> /dev/null
	@-rm -f $(RELEASE_FILE) 2> /dev/null


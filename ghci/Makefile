# Scripts for building and other things.
# The script takes into account Win platforms,
# but you do need MSYS2 for it to run properly.


.PHONY: help clean cleanall deploy release vartest


# Choose an extension appropiate to the OS
OWN_DIR			:= ghci
EXE_EXT 		:= .bin
DIR_SEP_CHAR		:= /
TEXMACS_PLUGIN_DIR	:= ${HOME}/.TeXmacs/plugins
COPY_CMD		:= cp
MKDIR_CMD		:= mkdir -p
DYNAMIC_LIBS		:= -dynamic
PIE			:= -fPIE
RELEASE_FILE		:= tm-ghci.tar.xz
PACK_CMD		:= tar -cJf $(RELEASE_FILE)

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
	DYNAMIC_LIBS		:=
	PIE			:=
	RELEASE_FILE		:= tm-ghci.zip
	PACK_CMD		:= zip -q9 $(RELEASE_FILE)
endif

BASE_NAME	:= GHCIInterface
SOURCE_FILE	:= src$(DIR_SEP_CHAR)$(BASE_NAME).hs
TARGET_EXE	:= bin$(DIR_SEP_CHAR)$(BASE_NAME)$(EXE_EXT)
DOC_FNAMES	:= ghci.en.tm ghci-abstract.en.tm ghci-demo.en.tm ghci-contact.en.tm haskell.png
DOC_FILES	:= $(foreach fname,$(DOC_FNAMES), doc$(DIR_SEP_CHAR)$(fname))
SCHEME_FILE	:= progs$(DIR_SEP_CHAR)init-ghci.scm

DEPLOY_DIR	:= $(TEXMACS_PLUGIN_DIR)$(DIR_SEP_CHAR)$(OWN_DIR)
DOC_DEPLOY_DIR	:= $(DEPLOY_DIR)$(DIR_SEP_CHAR)doc

DEPLOY_TARGET_EXE	:= $(DEPLOY_DIR)$(DIR_SEP_CHAR)$(TARGET_EXE)
DEPLOY_DOC_FILES	:= $(foreach fpath,$(DOC_FILES),$(DEPLOY_DIR)$(DIR_SEP_CHAR)$(fpath))
DEPLOY_SCHEME_FILE	:= $(DEPLOY_DIR)$(DIR_SEP_CHAR)$(SCHEME_FILE)


$(TARGET_EXE): $(SOURCE_FILE)
	@echo :: Compiling
	ghc -O2 -g0 $(DYNAMIC_LIBS) $(PIE) $< -o $@
	@echo :: Stripping
	strip -s -x -w -R .comment -R .note\* $@

.ONESHELL:
help:
	@echo "Calling make without arguments just builds"
	@echo "the executable $(TARGET_EXE)."
	@echo
	@echo "Call with 'clean'    to remove temp files"
	@echo "Call with 'cleanall' to remove temp files and executables"

.ONESHELL:
vartest:
	@echo USERPROFILE         = $(USERPROFILE)
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
	@echo RELEASE_FILE        = $(RELEASE_FILE)
	@echo PACK_CMD            = $(PACK_CMD)

clean:
	-@rm $(SET_FILE)
	-@cd src && rm *.hi *.o *~

cleanall: clean
	-@cd bin && rm -f *

$(DEPLOY_TARGET_EXE) $(DEPLOY_DOC_FILES) $(DEPLOY_SCHEME_FILE): $(TARGET_EXE) $(DOC_FILES) $(SCHEME_FILE)
	@echo :: Deploying
	$(MKDIR_CMD) $(DEPLOY_DIR)$(DIR_SEP_CHAR){bin,doc,progs}
	@$(COPY_CMD) $(TARGET_EXE) $(DEPLOY_TARGET_EXE)
	@$(COPY_CMD) $(SCHEME_FILE) $(DEPLOY_SCHEME_FILE)
	$(COPY_CMD) $(DOC_FILES) $(DOC_DEPLOY_DIR)

deploy: $(DEPLOY_TARGET_EXE) $(DEPLOY_DOC_FILE) $(DEPLOY_DOC_IMG_FILE) $(DEPLOY_SCHEME_FILE)

.ONESHELL:
..$(DIR_SEP_CHAR)$(RELEASE_FILE): $(TARGET_EXE) $(DOC_FILES) $(SCHEME_FILE)
	@cd ..
	@-rm ../$(RELEASE_FILE) &> /dev/null
	$(PACK_CMD) $(foreach file,$^,ghci/$(file))

release: ..$(DIR_SEP_CHAR)$(RELEASE_FILE)

# Scripts for building and other things.
# The script takes into account Win platforms,
# but you do need MSYS2 for it to run properly.


.PHONY: help clean cleanall deploy vartest


# Choose an extension appropiate to the OS
OWN_DIR			:= ghci
EXE_EXT 		:= .bin
DIR_SEP_CHAR		:= /
TEXMACS_PLUGIN_DIR	:= ${HOME}/.TeXmacs/plugins
COPY_CMD		:= cp
MKDIR_CMD		:= mkdir -p
DYNAMIC_LIBS		:= -dynamic
PIE			:= -fPIE

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
endif

BASE_NAME	:= GHCIInterface
SOURCE_FILE	:= src$(DIR_SEP_CHAR)$(BASE_NAME).hs
TARGET_EXE	:= bin$(DIR_SEP_CHAR)$(BASE_NAME)$(EXE_EXT)
DOC_FILE	:= doc$(DIR_SEP_CHAR)Help\ -\ The\ GHCi\ plug-in.tm
DOC_IMG_FILE	:= doc$(DIR_SEP_CHAR)haskell.png
SCHEME_FILE	:= progs$(DIR_SEP_CHAR)init-ghci.scm

DEPLOY_DIR	:= $(TEXMACS_PLUGIN_DIR)$(DIR_SEP_CHAR)$(OWN_DIR)

DEPLOY_TARGET_EXE	:= $(DEPLOY_DIR)$(DIR_SEP_CHAR)$(TARGET_EXE)
DEPLOY_DOC_FILE		:= $(DEPLOY_DIR)$(DIR_SEP_CHAR)$(DOC_FILE)
DEPLOY_DOC_IMG_FILE	:= $(DEPLOY_DIR)$(DIR_SEP_CHAR)$(DOC_IMG_FILE)
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
	@echo DOC_FILE            = $(DOC_FILE)
	@echo DOC_IMG_FILE        = $(DOC_IMG_FILE)
	@echo SCHEME_FILE         = $(SCHEME_FILE)
	@echo DEPLOY_TARGET_EXE   = $(DEPLOY_TARGET_EXE)
	@echo DEPLOY_DOC_FILE     = $(DEPLOY_DOC_FILE)
	@echo DEPLOY_DOC_IMG_FILE = $(DEPLOY_DOC_IMG_FILE)
	@echo DEPLOY_SCHEME_FILE  = $(DEPLOY_SCHEME_FILE)

clean:
	-@rm $(SET_FILE)
	-@cd src && rm *.hi *.o *~

cleanall: clean
	-@cd bin && rm -f *

$(DEPLOY_TARGET_EXE) $(DEPLOY_DOC_FILE) $(DEPLOY_IMG_FILE) $(DEPLOY_SCHEME_FILE): $(TARGET_EXE) $(DOC_FILE) $(DOC_IMG_FILE) $(SCHEME_FILE)
	@echo :: Deploying
	$(MKDIR_CMD) $(DEPLOY_DIR)$(DIR_SEP_CHAR){bin,doc,progs}
	@$(COPY_CMD) $(TARGET_EXE) $(DEPLOY_TARGET_EXE)
	@$(COPY_CMD) $(SCHEME_FILE) $(DEPLOY_SCHEME_FILE)
	@$(COPY_CMD) $(DOC_FILE) $(DEPLOY_DOC_FILE)
	@$(COPY_CMD) $(DOC_IMG_FILE) $(DEPLOY_DOC_IMG_FILE)

deploy: $(DEPLOY_TARGET_EXE) $(DEPLOY_DOC_FILE) $(DEPLOY_DOC_IMG_FILE) $(DEPLOY_SCHEME_FILE)


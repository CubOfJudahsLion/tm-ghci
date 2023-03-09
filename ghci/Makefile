# Scripts for building and other things.
# The script takes into account Win platforms,
# but you do need MSYS2 for it to run properly.


.PHONY: help clean cleanall deploy test


# Choose an extension appropiate to the OS
OWN_DIR			:= ghci
EXE_EXT 		:= .bin
DIR_SEP_CHAR		:= /
TEXMACS_PLUGIN_DIR	:= ${HOME}/.TeXmacs/plugins
COPY_CMD		:= cp -R
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

DEPLOY_DIR	:= "$(TEXMACS_PLUGIN_DIR)$(DIR_SEP_CHAR)$(OWN_DIR)"


$(TARGET_EXE): $(SOURCE_FILE)
	@echo :: Compiling
	ghc -O2 -g0 $< $(DYNAMIC_LIBS) $(PIE) -o $@
	@echo :: Stripping
	strip -s -x -w -R .comment -R .note\* $@
	@echo ":: Done"

.ONESHELL:
help:
	@echo "Calling make without arguments just builds"
	@echo "the executable $(TARGET_EXE)."
	@echo
	@echo "Call with 'clean'    to remove temp files"
	@echo "Call with 'cleanall' to remove temp files and executables"

.ONESHELL:
test:
	@echo USERPROFILE        = $(USERPROFILE)
	@echo EXE_EXT            = $(EXE_EXT)
	@echo DIR_SEP_CHAR       = $(DIR_SEP_CHAR)
	@echo TEXMACS_PLUGIN_DIR = $(TEXMACS_PLUGIN_DIR)
	@echo COPY_CMD           = $(COPY_CMD)
	@echo MKDIR_CMD          = $(MKDIR_CMD)
	@echo SHARED             = $(SHARED)
	@echo PIE                = $(PIE)

clean:
	-@rm $(SET_FILE)
	-@cd src && rm *.hi *.o *~

cleanall: clean
	-@cd bin && rm *$(EXE_EXT)

deploy: $(TARGET_EXE) progs$(DIR_SEP_CHAR)init-ghci.scm doc$(DIR_SEP_CHAR)Help\ -\ The\ GHCi\ plug-in.tm
	@echo :: Deploying
	@$(MKDIR_CMD) $(DEPLOY_DIR)
	@$(COPY_CMD) bin doc progs $(DEPLOY_DIR)


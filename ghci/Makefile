.PHONY: help, clean, deploy, test


# Choose an extension appropiate to the OS
EXE_EXT 		:= .bin
DIR_SEP_CHAR		:= /
TEXMACS_PLUGIN_DIR	:= /home/${USER}/.TeXmacs/plugins
COPY_CMD		:= cp -f -R
MKDIR_CMD		:= mkdir -p

ifneq ("${SYSTEMROOT}","")
	DIR_SEP_CHAR		:= \\
	EXE_EXT			:= .exe
	TEXMACS_PLUGIN_DIR	:= $(USERPROFILE)\\AppData\\Roaming\\TeXmacs\\plugins
endif

ifneq ("${HOME}","/home/${USER}")
	COPY_CMD		:= robocopy /Y
	MKDIR_CMD		:= mkdir
endif

# Designate the target accordingly
TARGET_EXE := bin$(DIR_SEP_CHAR)Interface$(EXE_EXT)


$(TARGET_EXE): src$(DIR_SEP_CHAR)Interface.hs
	@echo ":: Compiling"
	@ghc -O2 -g0 $? -threaded -o $@
	@echo ":: Stripping"
	@strip -R .comments -s -x -g --strip-unneeded $@
	@echo ":: Done"

.ONESHELL:
help:
	@echo "Calling make without arguments just builds"
	@echo "the executable $(TARGET_EXE)."
	@echo
	@echo "Call with 'clean'  to remove temp files"

.ONESHELL:
test:
	@echo $$ USERPROFILE        = $(USERPROFILE)
	@echo $$ EXE_EXT            = $(EXE_EXT)
	@echo $$ DIR_SEP_CHAR       = $(DIR_SEP_CHAR)
	@echo $$ TEXMACS_PLUGIN_DIR = $(TEXMACS_PLUGIN_DIR)
	@echo $$ COPY_CMD           = $(COPY_CMD)
	@echo $$ MKDIR_CMD          = $(MKDIR_CMD)

.ONESHELL:
clean:
	@cd src
	-@rm *.hi *.o *~

deploy:
	-@ $(MKDIR) $(TEXMACS_PLUGIN_DIR)
	$(COPY_CMD) ghci $(TEXMACS_PLUGIN_DIR)

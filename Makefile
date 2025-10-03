#! /usr/bin/make

# Requirements: pandoc, pdflatex, git, base development packages

MACHINE			:= $(shell uname -m | sed -re 's/^(.*)$$/\L\1/')
OS			:= $(shell uname -s | sed -re 's/^(.*)$$/\L\1/')
VERSION			:= $(shell git tag -l "v*" | tail -n 1 | sed -re 's/^(.*)$$/\L\1/')

PROFILE_PLUGINS_DIR	:= $(HOME)/.TeXmacs/plugins
DIR_SEP_CHAR		:= /

# Windows variables
ifneq ("$(USERPROFILE)","")
	PROFILE_PLUGINS_DIR := $(USERPROFILE)\\AppData\\Roaming\\TeXmacs\\plugins
	DIR_SEP_CHAR := \\
endif

.PHONY: help readme testvars

help:
	@echo Type:
	@echo "- 'make readme'  to create the README files"
	@echo "- 'make release' to create a release archive"
	@echo "- 'make deploy'  to install the plugin in $(PROFILE_PLUGINS_DIR)"

readme:
	cd readme-src && $(MAKE)

testvars:
	@echo "DIR_SEP_CHAR       : $(DIR_SEP_CHAR)"
	@echo "MACHINE            : $(MACHINE)"
	@echo "OS                 : $(OS)"
	@echo "PROFILE_PLUGINS_DIR: $(PROFILE_PLUGINS_DIR)"
	@echo "VERSION            : $(VERSION)"


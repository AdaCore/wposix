############################################################################
#                                 wPOSIX                                   #
#                                                                          #
#                    Copyright (C) 2008-2013, AdaCore                      #
#                                                                          #
#  This is free software;  you can redistribute it  and/or modify it       #
#  under terms of the  GNU General Public License as published  by the     #
#  Free Software  Foundation;  either version 3,  or (at your option) any  #
#  later version.  This software is distributed in the hope  that it will  #
#  be useful, but WITHOUT ANY WARRANTY;  without even the implied warranty #
#  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU     #
#  General Public License for  more details.                               #
#                                                                          #
#  You should have  received  a copy of the GNU General  Public  License   #
#  distributed  with  this  software;   see  file COPYING3.  If not, go    #
#  to http://www.gnu.org/licenses for a complete copy of the license.      #
############################################################################

.SILENT:

############################################################################
#  Default configuration
VERSION		= 1.2w

prefix	 	= $(dir $(shell which gnatls))..
ENABLE_SHARED 	= true
DEFAULT_LIBRARY_TYPE 	= static
PROCESSORS	= 0
DEBUG		= false
TARGET		= $(shell gcc -dumpmachine)

-include makefile.setup

HOST		= $(shell gcc -dumpmachine)

MODE            = $(if $(filter-out true,$(DEBUG)),release,debug)
SDIR            = $(TARGET)/$(MODE)

ifeq ($(DEBUG), true)
GPROPTS		= -XPRJ_BUILD=Debug
else
GPROPTS		= -XPRJ_BUILD=Release
endif

ifeq ($(HOST), $(TARGET))
TPREFIX		= $(prefix)
else
GPROPTS		+= --target=$(TARGET)
TPREFIX		= $(prefix)/$(TARGET)
endif

MKDIR		= mkdir
CP		= cp -p
GPRBUILD	= gprbuild
GPRINSTALL	= gprinstall
GPRCLEAN	= gprclean
RM		= rm -f
PYTHON		= python

############################################################################

all: build

#######################################################################
#  setup

setup: gen_setup

gen_setup:
	echo "prefix=$(prefix)" > makefile.setup
	echo "DEFAULT_LIBRARY_TYPE=$(DEFAULT_LIBRARY_TYPE)" >> makefile.setup
	echo "ENABLE_SHARED=$(ENABLE_SHARED)" >> makefile.setup
	echo "DEBUG=$(DEBUG)" >> makefile.setup
	echo "PROCESSORS=$(PROCESSORS)" >> makefile.setup
	echo "TARGET=$(TARGET)" >> makefile.setup

#######################################################################
#  install

install-clean:
	-$(GPRINSTALL) $(GPROPTS) -q --uninstall --prefix=$(TPREFIX) -Pwposix

install: install-clean
	$(GPRINSTALL) $(GPROPTS) -p -f --prefix=$(TPREFIX) \
		--subdirs=$(SDIR)/static -XLIBRARY_TYPE=static -Pwposix
ifeq (${ENABLE_SHARED}, true)
	$(GPRINSTALL) $(GPROPTS) -p -f --prefix=$(TPREFIX) \
		--subdirs=$(SDIR)/relocatable -XLIBRARY_TYPE=relocatable \
		--build-name=relocatable -Pwposix
endif

#######################################################################
#  build

build:
	$(GPRBUILD) -p $(GPROPTS) -j$(PROCESSORS) \
		--subdirs=$(SDIR)/static -XLIBRARY_TYPE=static -Pwposix
ifeq (${ENABLE_SHARED}, true)
	$(GPRBUILD) -p $(GPROPTS) -j$(PROCESSORS) \
		--subdirs=$(SDIR)/relocatable -XLIBRARY_TYPE=relocatable \
		-Pwposix
endif

#######################################################################
#  clean

clean:
	-$(GPRCLEAN) $(GPROPTS) -XLIBRARY_TYPE=static \
		--subdirs=$(SDIR)/static -Pwposix
ifeq (${ENABLE_SHARED}, true)
	-$(GPRCLEAN) $(GPROPTS) -XLIBRARY_TYPE=relocatable \
		--subdirs=$(SDIR)/relocatable -Pwposix
endif
	$(RM) -fr .build makefile.setup

run_regtests:
	(cd regtests; $(PYTHON) ./testsuite.py)

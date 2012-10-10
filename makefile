############################################################################
#                                 wPOSIX                                   #
#                                                                          #
#                    Copyright (C) 2008-2012, AdaCore                      #
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
VERSION		= 1.0

prefix	 	= $(dir $(shell which gnatls))..
ENABLE_SHARED 	= true
DEFAULT_LIBRARY_TYPE 	= static
PROCESSORS	= 0
DEBUG		= false
TARGET		= $(shell gcc -dumpmachine)

-include makefile.setup

HOST		= $(shell gcc -dumpmachine)

GCLOPTS		= -XTARGET=$(TARGET)
GPROPTS		= -j$(PROCESSORS) $(GCLOPTS)

BUILD   	= .build/$(TARGET)
CONFGPR 	= $(BUILD)/projects/wposix_config.gpr

ifeq ($(HOST), $(TARGET))
TPREFIX=$(prefix)
else
GPROPTS		+= --target=$(TARGET)
TPREFIX=$(prefix)/$(TARGET)
endif

MKDIR		= mkdir
CP		= cp -p
GPRBUILD	= gprbuild
GPRCLEAN	= gprclean
RM		= rm -f
LN		= ln -s

ifeq ($(DEBUG), true)
BDIR		= $(BUILD)/debug
GPROPTS		+= -XPRJ_BUILD=Debug
else
BDIR		= $(BUILD)/release
GPROPTS		+= -XPRJ_BUILD=Release
endif

PYTHON		= python

############################################################################

all: build

setup: setup_dirs gen_setup

setup_dirs:
	$(MKDIR) -p $(BUILD)/projects/

gen_setup:
	echo "prefix=$(prefix)" > makefile.setup
	echo "DEFAULT_LIBRARY_TYPE=$(DEFAULT_LIBRARY_TYPE)" >> makefile.setup
	echo "ENABLE_SHARED=$(ENABLE_SHARED)" >> makefile.setup
	echo "DEBUG=$(DEBUG)" >> makefile.setup
	echo "PROCESSORS=$(PROCESSORS)" >> makefile.setup
	echo "TARGET=$(TARGET)" >> makefile.setup
#  Generate config for install
	echo 'project wPOSIX_Config is' > $(CONFGPR)
	echo '   for Source_Dirs use ();' >> $(CONFGPR)
	echo '   Default_Library_Type := "'$(DEFAULT_LIBRARY_TYPE)'";' \
		>> $(CONFGPR)
	echo 'end wPOSIX_Config;' >> $(CONFGPR)

install:
	$(MKDIR) -p $(TPREFIX)/lib/gnat/wposix
	$(MKDIR) -p $(TPREFIX)/lib/wposix/static
	$(CP) -pr $(BDIR)/static/lib/* $(TPREFIX)/lib/wposix/static/
ifeq (${ENABLE_SHARED}, true)
	$(MKDIR) -p $(TPREFIX)/lib/wposix/relocatable
	$(CP) -pr $(BDIR)/relocatable/lib/* $(TPREFIX)/lib/wposix/relocatable/
endif
	$(MKDIR) -p $(TPREFIX)/include/wposix
	$(CP) -p src/*.ad* $(TPREFIX)/include/wposix/
	$(CP) $(CONFGPR) $(TPREFIX)/lib/gnat/wposix/
	$(CP) config/projects/wposix.gpr $(TPREFIX)/lib/gnat/
	$(CP) config/projects/wposix_shared.gpr $(TPREFIX)/lib/gnat/wposix/

build:
	$(GPRBUILD) -p $(GPROPTS) -XLIBRARY_TYPE=static -P wposix
ifeq (${ENABLE_SHARED}, true)
	$(GPRBUILD) -p $(GPROPTS) -XLIBRARY_TYPE=relocatable -P wposix
endif

clean:
	-$(GPRCLEAN) $(GCLOPTS) -XLIBRARY_TYPE=static -P wposix
ifeq (${ENABLE_SHARED}, true)
	-$(GPRCLEAN) $(GCLOPTS) -XLIBRARY_TYPE=relocatable -P wposix
endif
	$(RM) -fr $(BUILD) makefile.setup

run_regtests:
	(cd regtests; $(PYTHON) ./testsuite.py)

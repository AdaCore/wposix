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

-include makefile.setup

BUILD   	= .build
CONFGPR 	= $(BUILD)/projects/wposix_config.gpr

MKDIR		= mkdir
CP		= cp -p
GNAT		= gnat
RM		= rm -f
LN		= ln -s

GMOPTS		= -XPROCESSORS=$(PROCESSORS)

ifeq ($(DEBUG), true)
BDIR		= $(BUILD)/debug
GMOPTS		:= $(GMOPTS) -XPRJ_BUILD=Debug
else
BDIR		= $(BUILD)/release
GMOPTS		:= $(GMOPTS) -XPRJ_BUILD=Release
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
#  Generate config for install
	echo 'project wPOSIX_Config is' > $(CONFGPR)
	echo '   for Source_Dirs use ();' >> $(CONFGPR)
	echo '   Default_Library_Type := "'$(DEFAULT_LIBRARY_TYPE)'";' \
		>> $(CONFGPR)
	echo 'end wPOSIX_Config;' >> $(CONFGPR)

install:
	$(MKDIR) -p $(prefix)/lib/gnat/wposix
	$(MKDIR) -p $(prefix)/lib/wposix/static
	$(CP) -pr $(BDIR)/static/lib/* $(prefix)/lib/wposix/static/
ifeq (${ENABLE_SHARED}, true)
	$(MKDIR) -p $(prefix)/lib/wposix/relocatable
	$(CP) -pr $(BDIR)/relocatable/lib/* $(prefix)/lib/wposix/relocatable/
endif
	$(MKDIR) -p $(prefix)/include/wposix
	$(CP) -p src/*.ad* $(prefix)/include/wposix/
	$(CP) $(CONFGPR) $(prefix)/lib/gnat/wposix/
	$(CP) config/projects/wposix.gpr $(prefix)/lib/gnat/
	$(CP) config/projects/wposix_shared.gpr $(prefix)/lib/gnat/wposix/

build:
	$(GNAT) make -p $(GMOPTS) -XLIBRARY_TYPE=static -P wposix
ifeq (${ENABLE_SHARED}, true)
	$(GNAT) make -p $(GMOPTS) -XLIBRARY_TYPE=relocatable -P wposix
endif

clean:
	$(GNAT) clean $(GMOPTS) -XLIBRARY_TYPE=static -P wposix
ifeq (${ENABLE_SHARED}, true)
	$(GNAT) clean $(GMOPTS) -XLIBRARY_TYPE=relocatable -P wposix
endif
	$(RM) -r $(BUILD)

run_regtests:
	(cd regtests; $(PYTHON) ./testsuite.py)

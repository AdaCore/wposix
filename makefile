
# $Id$

.SILENT: all distrib

all:
	echo ""
	echo You must provide a target:
	echo ""
	echo "  build   : build the POSIX binding"
	echo "  distrib : build a new distribution"
	echo ""

distrib:
	-rm win32posix.tar.gz
	tar cf win32posix.tar src/compile.bat src/*.ad[sb] \
		src/readme.txt test/demo*.adb

	gzip -9 win32posix.tar

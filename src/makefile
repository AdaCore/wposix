
# $Id$

.SILENT:

INSTALL_DIR	=	/usr/Ada.Libraries/POSIX

all:	src

src:	force
	gnatmake -c posix.adb
	gnatmake -c posix_calendar.adb
	gnatmake -c posix_file_status.adb
	gnatmake -c posix_files.adb
	gnatmake -c posix_io.adb
	gnatmake -c posix_permissions.adb
	gnatmake -c posix_process_environment.adb
	gnatmake -c posix_process_identification.adb
	gnatmake -c posix_process_primitives.adb
	gnatmake -c posix_process_times.adb
	gnatmake -c posix_signals.adb
	gnatmake -c posix_win32-file_handle.adb
	gnatmake -c posix_win32.adb

force:

distrib:
	cp *.ad[sb] $(INSTALL_DIR)
	cp *.{o,ali} $(INSTALL_DIR)


# $Id$

.SILENT:

GFLAGS		=	-I- -I. -O2 -gnaty3abcehiklmptrf -gnatwu
INSTALL_DIR	=	/usr/Ada.Libraries/POSIX

all:	src

src:	force
	gnatmake -c $(GFLAGS) posix.adb
	gnatmake -c $(GFLAGS) posix_calendar.adb
	gnatmake -c $(GFLAGS) posix_file_status.adb
	gnatmake -c $(GFLAGS) posix_files.adb
	gnatmake -c $(GFLAGS) posix_io.adb
	gnatmake -c $(GFLAGS) posix_permissions.adb
	gnatmake -c $(GFLAGS) posix_process_environment.adb
	gnatmake -c $(GFLAGS) posix_process_identification.adb
	gnatmake -c $(GFLAGS) posix_process_primitives.adb
	gnatmake -c $(GFLAGS) posix_process_times.adb
	gnatmake -c $(GFLAGS) posix_signals.adb
	gnatmake -c $(GFLAGS) posix_win32-file_handle.adb
	gnatmake -c $(GFLAGS) posix_win32.adb

force:

clean:
	rm *.ali *.o

install:
	cp *.ad[sb] $(INSTALL_DIR)
	cp *.{o,ali} $(INSTALL_DIR)

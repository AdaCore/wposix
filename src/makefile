
# $Id$

.SILENT:

GFLAGS		=	-I- -I. -O2 -gnaty3abcehiklmptrf -gnatwu
INSTALL_DIR	=	/usr/Ada.Libraries/POSIX

all:	src

src:	force
	gnatmake -c $(GFLAGS) posix.adb
	gnatmake -c $(GFLAGS) posix-calendar.adb
	gnatmake -c $(GFLAGS) posix-file_status.adb
	gnatmake -c $(GFLAGS) posix-files.adb
	gnatmake -c $(GFLAGS) posix-io.adb
	gnatmake -c $(GFLAGS) posix-permissions.adb
	gnatmake -c $(GFLAGS) posix-process_environment.adb
	gnatmake -c $(GFLAGS) posix-process_identification.adb
	gnatmake -c $(GFLAGS) posix-process_primitives.adb
	gnatmake -c $(GFLAGS) posix-process_times.adb
	gnatmake -c $(GFLAGS) posix-signals.adb
	gnatmake -c $(GFLAGS) posix_calendar.ads
	gnatmake -c $(GFLAGS) posix_file_status.ads
	gnatmake -c $(GFLAGS) posix_files.ads
	gnatmake -c $(GFLAGS) posix_io.ads
	gnatmake -c $(GFLAGS) posix_permissions.ads
	gnatmake -c $(GFLAGS) posix_process_environment.ads
	gnatmake -c $(GFLAGS) posix_process_identification.ads
	gnatmake -c $(GFLAGS) posix_process_primitives.ads
	gnatmake -c $(GFLAGS) posix_process_times.ads
	gnatmake -c $(GFLAGS) posix_signals.ads
	gnatmake -c $(GFLAGS) posix_win32-file_handle.adb
	gnatmake -c $(GFLAGS) posix_win32.adb

force:

clean:
	rm *.ali *.o

install:
	cp -p *.ad[sb] $(INSTALL_DIR)
	cp -p *.?(o|ali) $(INSTALL_DIR)

@echo off

if "%1" == "" goto errorconf

set PREFIX=%1
set S_GPR=%1\share\gpr\wposix.gpr

rem ----------------------------------------------- UNINSTALL

if exist %S_GPR% gprinstall --uninstall --prefix=%PREFIX% wposix.gpr

rem ----------------------------------------------- BUILD

gprbuild -j0 -p -XPRJ_BUILD=Release -XLIBRARY_TYPE=static --subdirs=static wposix.gpr
gprbuild -j0 -p -XPRJ_BUILD=Release -XLIBRARY_TYPE=relocatable --subdirs=relocatable -XWIN32ADA_BUILD=relocatable wposix.gpr

rem ----------------------------------------------- INSTALL

gprinstall -p --prefix=%PREFIX% -XLIBRARY_TYPE=static --subdirs=static wposix.gpr
gprinstall -p --prefix=%PREFIX% -XLIBRARY_TYPE=relocatable --subdirs=relocatable --build-name=relocatable -XWIN32ADA_BUILD=relocatable wposix.gpr

goto end

:errorconf
echo Missing GNAT root as parameter
echo c:^> install c:\gnatpro\7.3.1
goto end

:end

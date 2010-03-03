@echo off

if "%1" == "" goto errorconf

set PREFIX=%1

gnat make -j2 -p -XPRJ_BUILD=Release -XLIBRARY_TYPE=static -P wposix
gnat make -j2 -p -XPRJ_BUILD=Release -XLIBRARY_TYPE=relocatable -P wposix

mkdir .build\projects 2> nul

set CONFGPR=.build\projects\wposix_config.gpr

echo project wPOSIX_Config is > %CONFGPR%
echo    for Source_Dirs use (); >> %CONFGPR%
echo    Default_Library_Type := "static"; >> %CONFGPR%
echo end wPOSIX_Config; >> %CONFGPR%

mkdir %PREFIX%\lib\gnat\wposix 2> nul
mkdir %PREFIX%\lib\wposix\static  2> nul
copy .build\release\static\lib\* %PREFIX%\lib\wposix\static\
mkdir %PREFIX%\lib\wposix\relocatable  2> nul
copy .build\release\relocatable\lib\* %PREFIX%\lib\wposix\relocatable\
mkdir %PREFIX%\include\wposix 2> nul
copy src\*.ad* %PREFIX%\include\wposix\
copy %CONFGPR% %PREFIX%\lib\gnat\wposix\
copy config\projects\wposix.gpr %PREFIX%\lib\gnat\
copy config\projects\wposix_shared.gpr %PREFIX%\lib\gnat\wposix\

goto end

:errorconf
echo Missing GNAT root as parameter
echo c:^> install c:\gnatpro\6.3.1
goto end

:end

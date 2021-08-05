@rem Assemble the installation package for 32-bit XRSound
@rem Usage: makedist_x86 version_number
@rem   e.g., makedist_x86 3.1
@rem D. Beachy, 1-Aug-2021
@rem ----------------------------------------------------
@echo off
setlocal
if not exist dist\XRSound mkdir dist\XRSound
set version=%1-x86
if '%version%' NEQ '-x86' GOTO version_ok
echo Usage: makedist_x86 ~version~; e.g., makedist 3.1
goto :eof

:version_ok
pushd dist\XRSound
@rem NOTE: workdir MUST BE one level under version dir for zip file creation to work!
set workdir=%version%\work
set SolutionDir=..\..\XRSound\src
set assetsdir=..\..\XRSound\assets
set irrKlangDLLdir=%IRRKLANG_ROOT%\bin\win32-visualStudio

echo Packaging 32-bit XRSound %version% release

@rem whack the work dir if it exists; do NOT whack the version directory! It may have other files in it!
if exist %workdir% rd /s /q %workdir%

@rem create base directory structure (excluding sound files)
mkdir %workdir%
mkdir %workdir%\Doc
mkdir %workdir%\Modules\Plugin
mkdir %workdir%\Orbitersdk\XRSound
mkdir %workdir%\XRSound

@rem copy FULL DISTRIBUTION files
echo Copying XRSound User Manual and $ORBIER_ROOT\XRSound\* files
call :copyfile "%assetsdir%\XRSound\*.cfg"                %workdir%\XRSound\*
call :copyfile "%assetsdir%\XRSound\ReadMe.txt"           %workdir%\XRSound\*
call :copyfile "%assetsdir%\Doc\XRSound User Manual.pdf"  %workdir%\Doc\*

@rem copy binaries, XRSound.h, and readme (note the copy-rename of XRSoundDLL.dll -> XRSound.dll)
@rem Also, we have to ship the debug version of XRSound.lib (XRSoundD.lib) and irrKlang.dll so the users can build debug versions of their ships.
call :copyfile "%SolutionDir%\release\XRSoundDLL.dll"     %workdir%\Modules\Plugin\XRSound.dll
call :copyfile "%SolutionDir%\release\XRSound.lib"        %workdir%\Orbitersdk\XRSound\*
call :copyfile "%SolutionDir%\debug\XRSound.lib"          %workdir%\Orbitersdk\XRSound\XRSoundD.lib
call :copyfile "%SolutionDir%\XRSoundLib\XRSound.h"       %workdir%\Orbitersdk\XRSound\*
call :copyfile "%irrKlangDLLdir%\ikpMP3.dll"              %workdir%\*
call :copyfile "%irrKlangDLLdir%\ikpFlac.dll"             %workdir%\*
call :copyfile "%irrKlangDLLdir%\irrKlang.dll"            %workdir%\*

@rem create the special DLL-only ZIP file (useful for unofficial releases such as a beta)
set zipfile=%version%\XRSound-%version%-dll.zip
if exist %zipfile% del %zipfile%
call WinRar a -afzip -ep -m5 %zipfile% "%workdir%\Modules\Plugin\XRSound.dll"

@rem create the special no-sound upgrade ZIP file (useful for unofficial releases such as a beta)
set zipfile=%version%\XRSound-%version%-no-sound-files.zip
if exist %zipfile% del "%zipfile%"
pushd %workdir%
call WinRar a -afzip -ep1 -m5 -r ..\XRSound-%version%-no-sound-files.zip *
popd

@rem create the sound files directory structure and copy the files
mkdir "%workdir%\XRSound\Default\Cabin Ambience"
mkdir "%workdir%\XRSound\Default\Altea STS"
mkdir "%workdir%\XRSound\Default\Music"
call :copyfile "%assetsdir%\XRSound\Default\*"                      "%workdir%\XRSound\Default\*"
call :copyfile "%assetsdir%\XRSound\Default\Altea STS\*"            "%workdir%\XRSound\Default\Altea STS\*"
call :copyfile "%assetsdir%\XRSound\Default\Cabin Ambience\*"       "%workdir%\XRSound\Default\Cabin Ambience\*"
call :copyfile "%assetsdir%\XRSound\Default\Music\*"                "%workdir%\XRSound\Default\Music\*"

@rem create the ZIP distribution file
set zipfile=%version%\XRSound-%version%.zip
if exist %zipfile% del %zipfile%
pushd %workdir%
@rem THIS ASSUMES WORKDIR IS ONE LEVEL UNDER VERSION DIR
call WinRar a -afzip -ep1 -m5 -r  ..\XRSound-%version%.zip *
popd

rem --------------------------------

dir %version%
popd
@echo makedist_x86 done!
@goto :eof

rem -------------
rem Copy a file after verifying it exists
rem -------------
:copyfile
  set src=%1
  set dest=%2
  echo "Copying %src% -> %dest%"
  if not exist %src% goto src_not_found
  copy %src% %dest%
  goto :eof

:src_not_found
  echo ERROR: source file '%src%' does not exist
  pause
  goto :eof

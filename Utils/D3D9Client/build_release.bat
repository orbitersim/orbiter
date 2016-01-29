@echo off
:: ----------------------------------------------------------------------------
:: Batch file to create a build package (ZIP) of the D3D9Client
::
:: Notes:
:: - you need to have 7-Zip installed (you might however still need to adjust
::   the ZIP_CMD setting below to make it work)
:: - The generated ZIP file name contains the revision number in brackets.
::   If that number ends with an 'M' (like e.g. r35884M), it means that it
::   was build containing local modifications!
::   This should *not* be a officially released version!
::
:: ----------------------------------------------------------------------------

:: --- Setup
set BASE_DIR=..\..
set OUT_DIR=_release
set VERSION=Beta17

:: Enhance Version by Orbiter Version
for /F "tokens=* USEBACKQ" %%i in (`over /N ..\..\Orbitersdk\lib\orbiter.lib`) do set OVER=%%i
set VERSION=%VERSION%-for%OVER%

:: Try to get the Visual Studio version
if not "%VS110COMNTOOLS%"=="" (
  set VSCOMNTOOLS="%VS110COMNTOOLS%"
  set SOLUTIONFILE=D3D9ClientVS2012.sln
  goto :assign
)
if not "%VS100COMNTOOLS%"=="" (
  set VSCOMNTOOLS="%VS100COMNTOOLS%"
  set SOLUTIONFILE=D3D9ClientVS2010.sln
  goto :assign
)
if not "%VS90COMNTOOLS%"=="" (
  set VSCOMNTOOLS="%VS90COMNTOOLS%"
  set SOLUTIONFILE=D3D9ClientVS2008.sln
  goto :assign
)

:assign
set ZIP_NAME=D3D9Client%VERSION%
set SETVCVARS=%VSCOMNTOOLS%vsvars32.bat
set VC=msbuild.exe
set BUILD_FLAG=/t:build
set SOLUTIONFILE="%BASE_DIR%\Orbitersdk\D3D9Client\%SOLUTIONFILE%"
set CONFIG=/p:Configuration=Release
set ZIP_CMD="C:\Program Files\7-Zip\7z.exe"


:: --- Update working copy & get revision number
svn update --quiet %BASE_DIR%
for /F "tokens=*" %%i IN ('svnversion %BASE_DIR%') DO set REVISION=%%i


:: --- Create folder structure
if exist "%OUT_DIR%" (
  rmdir /S /Q "%OUT_DIR%"
)
mkdir "%OUT_DIR%"


:: --- Build
call %SETVCVARS% x86
call %VC% %BUILD_FLAG% %SOLUTIONFILE% %CONFIG%


:: --- Export
set ABS_PATH=%cd%
svn export --force --quiet "%BASE_DIR%\Config" "%ABS_PATH%\%OUT_DIR%\Config"
svn export --force --quiet "%BASE_DIR%\Doc" "%ABS_PATH%\%OUT_DIR%\Doc"
svn export --force --quiet "%BASE_DIR%\Meshes" "%ABS_PATH%\%OUT_DIR%\Meshes"
svn export --force --quiet "%BASE_DIR%\Modules" "%ABS_PATH%\%OUT_DIR%\Modules"
svn export --force --quiet "%BASE_DIR%\Orbitersdk" "%ABS_PATH%\%OUT_DIR%\Orbitersdk"
svn export --force --quiet "%BASE_DIR%\Textures" "%ABS_PATH%\%OUT_DIR%\Textures"
svn export --force --quiet "%BASE_DIR%\Utils" "%ABS_PATH%\%OUT_DIR%\Utils"
svn export --force --quiet "%BASE_DIR%\D3D9Client.cfg" "%ABS_PATH%\%OUT_DIR%"


:: --- Copy the DLL
if not exist "%OUT_DIR%\Modules\Plugin" mkdir "%OUT_DIR%\Modules\Plugin"
copy /y %BASE_DIR%\Modules\Plugin\D3D9Client.dll ^
         %OUT_DIR%\Modules\Plugin\D3D9Client.dll > nul


:: --- Packing
cd %OUT_DIR%
%ZIP_CMD% a -tzip -mx9 "..\%ZIP_NAME%(r%REVISION%)+src.zip" *

rmdir /S /Q "Orbitersdk"
rmdir /S /Q "Utils"
%ZIP_CMD% a -tzip -mx9 "..\%ZIP_NAME%(r%REVISION%).zip" *


:: --- Cleanup & exit
cd %ABS_PATH%
rmdir /S /Q "%OUT_DIR%"
set BASE_DIR=
set OUT_DIR=
set VERSION=
set REVISION=
set SETVCVARS=
set VC=
set BUILD_FLAG=
set SOLUTIONFILE=
set CONFIG=
set ABS_PATH=
set ZIP_CMD=
set ZIP_NAME=
set SETVCVARS=
set VSCOMNTOOLS=

exit /b 0
:: ----------------------------------------------------------------------------
:: Batch file to get the VS150COMNTOOLS environment variable by asking the
:: Visual Studio 2017 (and greater) tool vswhere.exe
::
:: Notes:
:: - After calling this script VS150COMNTOOLS is either set (Visual Studio 2017
::   was found) or not set (no Visual Studio 2017 found)
:: - Heavily based on boost's "vswhere_usability_wrapper.cmd" scrit
::
:: ----------------------------------------------------------------------------
@echo off
setlocal

:: vswhere should be at Installer path
set "InstallerPath=%ProgramFiles(x86)%\Microsoft Visual Studio\Installer"
if not exist "%InstallerPath%" set "InstallerPath=%ProgramFiles%\Microsoft Visual Studio\Installer"
if not exist "%InstallerPath%" goto no-vswhere

:: Manipulate %Path% for easier " handeling
set Path=%Path%;%InstallerPath%
where vswhere 2> nul > nul
if errorlevel 1 goto no-vswhere

:: Setup arguments vor vswhere
set VSWHERE_REQ=-requires Microsoft.VisualStudio.Component.VC.Tools.x86.x64
set VSWHERE_PRP=-property installationPath
set VSWHERE_ARGS=-latest -products * %VSWHERE_REQ% %VSWHERE_PRP%

for /f "usebackq tokens=*" %%i in (`vswhere %VSWHERE_ARGS%`) do (
    endlocal
    set "VS150COMNTOOLS=%%i\Common7\Tools\"
    exit /B 0
)

:no-vswhere
endlocal
exit /B 1

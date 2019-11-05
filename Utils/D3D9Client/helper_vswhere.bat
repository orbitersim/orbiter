:: ----------------------------------------------------------------------------
:: Batch file to get the VS141COMNTOOLS rsp. VS142COMNTOOLS environment
:: variable by asking the Visual Studio 2017 (or greater) tool vswhere.exe
::
:: Notes:
:: - After calling this script either
::   VS141COMNTOOLS is set (Visual Studio 2017 was found) or
::   VS142COMNTOOLS is set (Visual Studio 2019 was found) or
::   none of these is set (no Visual Studio greater than 2017 found)
:: - Heavily based on boost's "vswhere_usability_wrapper.cmd" script
::
:: ----------------------------------------------------------------------------
@echo off
setlocal

:: Uncomment this to also recognize pre-release versions
:: set "PRERELEASE=-prerelease"

:: vswhere should be at Installer path
set "InstallerPath=%ProgramFiles(x86)%\Microsoft Visual Studio\Installer"
if not exist "%InstallerPath%" set "InstallerPath=%ProgramFiles%\Microsoft Visual Studio\Installer"
if not exist "%InstallerPath%" goto no-vswhere

:: Manipulate %Path% for easier handling
set Path=%Path%;%InstallerPath%
where vswhere 2> nul > nul
if errorlevel 1 goto no-vswhere

:: Setup arguments vor vswhere to get ProductLineVersion (2017 or 2019) 
set VSWHERE_REQ=-requires Microsoft.VisualStudio.Component.VC.Tools.x86.x64
set VSWHERE_PRP=-property catalog_productLineVersion
set VSWHERE_ARGS=-latest -products * %PRERELEASE% %VSWHERE_REQ% %VSWHERE_PRP%
:: execute vswhere query
for /f "usebackq tokens=*" %%i in (`vswhere %VSWHERE_ARGS%`) do (
    set "ProductLineVersion=%%i"
)

:: Setup arguments vor vswhere to get InstallationPath 
set VSWHERE_PRP=-property installationPath
set VSWHERE_ARGS=-latest -products * %PRERELEASE% %VSWHERE_REQ% %VSWHERE_PRP%
:: execute vswhere query
for /f "usebackq tokens=*" %%i in (`vswhere %VSWHERE_ARGS%`) do (
    endlocal
    if %ProductLineVersion% == 2017 set "VS141COMNTOOLS=%%i\Common7\Tools\"
    if %ProductLineVersion% == 2019 set "VS142COMNTOOLS=%%i\Common7\Tools\"
    exit /B 0
)

:no-vswhere
endlocal
exit /B 1

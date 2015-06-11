@echo off
:: ----------------------------------------------------------------------------
:: Batch file to (re-)create the D3D9Client_API_Reference.chm
::
:: Notes:
:: - you need to have doxygen (rsp.doxywizzard) installed (you might however
::   still need to adjust the DOXYGEN search below to make it work)
::
:: ----------------------------------------------------------------------------


::
:: Try to find doxygen...
::
for %%X in (doxygen.exe) do (set FOUND=%%~$PATH:X)
if defined FOUND (
  set DOXYGEN=doxygen.exe
  goto :run
)
:: or maybe here...
if exist "c:\Program Files\doxygen\bin\doxygen.exe" (
  set DOXYGEN="c:\Program Files\doxygen\bin\doxygen.exe"
  goto :run
)
:: last try here...
if exist "c:\Program Files (x86)\doxygen\bin\doxygen.exe" (
  set DOXYGEN="c:\Program Files (x86)\doxygen\bin\doxygen.exe"
  goto :run
)

:: --- failed to find doxygen ---
echo.
echo Sorry, could not find doxygen.exe anywhere!
echo   To create the API from the source^(s^) doxygen.exe
echo   ^(rsp. doxywizzard^) has to be installed ^(http://doxygen.org^)!
echo.
echo.
pause
goto :eof


:: --- Do the job ---
:run

::( type Doxyfile & echo PROJECT_NUMBER="Beta 12" ) | %DOXYGEN% -
%DOXYGEN% Doxyfile

:: --- clean up ---
set FOUND=
set DOXYGEN=
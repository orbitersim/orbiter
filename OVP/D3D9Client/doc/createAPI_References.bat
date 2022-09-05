@echo off
setlocal
:: ----------------------------------------------------------------------------
:: Batch file to (re-)create the D3D9Client_API_Reference.chm
::                           and gcAPI.chm files
::
:: Notes:
:: - you need to have doxygen (rsp.doxywizzard) installed (you might however
::   still need to adjust the doxygen search below to make it work)
::
:: ----------------------------------------------------------------------------


::
:: Try to find doxygen...
::
for %%X in (doxygen.exe) do (set FOUND=%%~$PATH:X)
if defined FOUND (
  set doxygen=doxygen.exe
  goto :find_dot
)
:: or maybe here...
for /f "delims=" %%i in ('dir /B "%ProgramFiles%"^|find /I "doxygen"') do (
  set doxygen="%ProgramFiles%\%%%i"
  goto :find_dot
)
:: last try here...
for /f "delims=" %%i in ('dir /B "%ProgramFiles(x86)%"^|find /I "doxygen"') do (
  set doxygen="%ProgramFiles(x86)%\%%%i"
  goto :find_dot
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


::
:: Try to find dot...
::
:find_dot
for %%X in (dot.exe) do (set FOUND_DOT=%%~$PATH:X)
if defined FOUND_DOT (
  goto :run
)
:: or maybe here...
for /f "delims=" %%i in ('dir /B "%ProgramFiles%"^|find /I "graphviz"') do (
  set graphviz_path="%ProgramFiles%\%%%i"
  goto :extend_path
)
:: last try here...
for /f "delims=" %%i in ('dir /B "%ProgramFiles(x86)%"^|find /I "graphviz"') do (
  set graphviz_path="%ProgramFiles(x86)%\%%%i"
  goto :extend_path
)

:: --- failed to find dot ---
echo.
echo Sorry, could not find dot.exe anywhere!
echo   To create the API from the source^(s^) dot.exe
echo   ^(rsp. graphviz^) has to be installed ^(http://graphviz.org^)!
echo.
echo.
pause
goto :eof


:: --- Extend PATH locally, so doxygen can use it ---
:extend_path
set graphviz_path=%graphviz_path:"=%\bin
::"
set "PATH=%path%;%graphviz_path%"


:: --- Do the jobs ---
:run

%doxygen% Doxyfile
%doxygen% Doxyfile-gcAPI

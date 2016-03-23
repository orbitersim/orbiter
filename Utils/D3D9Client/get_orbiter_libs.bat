@echo off
:: ----------------------------------------------------------------------------
:: Batch file to retreive (update) the needed orbiter SDK libraries and
:: headers to build the D3D9Client.
::
:: Notes:
:: - This swill always get the HEAD revision of Orbiter trunk
::   (see REV variable at setup)!
::
:: ----------------------------------------------------------------------------


:: --- Setup
set BASE_DIR=..\..
set URL=svn://orbiter-forum.com/orbiter/Orbitersdk
set REV=HEAD
set OUT_DIR=Orbitersdk


:: --- Do it
set OUT_DIR="%BASE_DIR%\%OUT_DIR%"
svn export --force -r %REV% %URL%/include %OUT_DIR%/include || goto exit_nok
svn export --force -r %REV% %URL%/lib     %OUT_DIR%/lib     || goto exit_nok


:: --- Pass / Fail exit
:exit_ok
call :cleanup
exit /B 0

:exit_nok
echo.
echo Failed to retrieve Orbiter SDK libs ^& headers!
call :cleanup
exit /B 1


:: --- Cleanup
:cleanup
set BASE_DIR=
set URL=
set REV=
set OUT_DIR=
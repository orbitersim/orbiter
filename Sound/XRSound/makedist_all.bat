@rem Assemble the 32-bit and 64-bit installation packages for XRSound
@rem Usage: makedist version_number
@rem   e.g., makedist 3.1
@rem D. Beachy, 1-Aug-2021
@rem ----------------------------------------------------
@echo off

set version=%1
if '%version%' NEQ '' GOTO version_ok
echo Usage: makedist_all ~version~; e.g., makedist_all 3.1
goto :eof

:version_ok
echo "Be sure you have built the latest binaries for both debug and release via Build -> Batch Build -> Select All except DEBUG XRSoundDLL ->  Build"
call makedist_x86.bat %1
call makedist_x64.bat %1

dir dist\XRSound\%1-x86 dist\XRSound\%1-x64
@echo makedist_all DONE!

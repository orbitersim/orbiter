@ECHO OFF
REM		       Dxsetenv.bat
REM
REM This is used to Set your Environment for DirectX Build.
REM
REM **Please set the below set statements to match your environment.**


REM set 32 bit C Compiler 
REM **Please change the below line to point to your C Compiler Path.**
set C32_ROOT=C:\PROGRA~1\DEVSTU~1\VC

REM Watcom
REM set C32_ROOT=c:\watcom

REM Borland
REM set C32_ROOT=c:\borland\cbuilder3

REM set DirectX SDK Path
REM **Please change the below line to point to your DirectX SDK Path.**
Set DXSDKROOT=C:\mssdk

REM **Please Do not Change the paths below.**

REM This is used to set up your Include and Lib Paths.

Set path=%C32_ROOT%\Bin;%C32_ROOT%\..\SharedIDE\Bin;%path%
Set include=%DXSDKROOT%\include;%C32_ROOT%\include;%C32_ROOT%\mfc\include;%include%
Set LIB=%DXSDKROOT%\lib;%C32_ROOT%\LIB;%C32_ROOT%\MFC\LIB;%LIB%;

REM Watcom
REM Set path=%C32_ROOT%\binnt;%path%
REM Set include=%DXSDKROOT%\include;%C32_ROOT%\h;%C32_ROOT%\h\nt;%C32_ROOT%\mfc\include;%include%
REM Set lib=%DXSDKROOT%\lib;%C32_ROOT%\lib386\nt;%lib%

REM Borland
REM Set path=%C32_ROOT%\bin;%path%
REM Set include=%DXSDKROOT%\include;%C32_ROOT%\include;%C32_ROOT%\include\mfc;%include%
REM Set lib=%DXSDKROOT%\lib\borland;%C32_ROOT%\lib;%lib%

REM Remove C32_ROOT
set C32_ROOT=

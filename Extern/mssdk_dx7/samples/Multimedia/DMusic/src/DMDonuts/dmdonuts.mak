# Microsoft Developer Studio Generated NMAKE File, Format Version 4.20
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Application" 0x0101

!IF "$(CFG)" == ""
CFG=dmdonuts - Win32 Release
!MESSAGE No configuration specified.  Defaulting to dmdonuts - Win32 Release.
!ENDIF 

!IF "$(CFG)" != "dmdonuts - Win32 Release" && "$(CFG)" !=\
 "dmdonuts - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "dmdonuts.mak" CFG="dmdonuts - Win32 Release"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "dmdonuts - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "dmdonuts - Win32 Debug" (based on "Win32 (x86) Application")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 
################################################################################
# Begin Project
# PROP Target_Last_Scanned "dmdonuts - Win32 Debug"
RSC=rc.exe
MTL=mktyplib.exe
CPP=cl.exe

!IF  "$(CFG)" == "dmdonuts - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
OUTDIR=.\Release
INTDIR=.\Release

ALL : "$(OUTDIR)\dmdonuts.exe"

CLEAN : 
	-@erase "$(INTDIR)\ddutil.obj"
	-@erase "$(INTDIR)\donuts.obj"
	-@erase "$(INTDIR)\donuts.res"
	-@erase "$(INTDIR)\dsutil.obj"
	-@erase "$(INTDIR)\input.obj"
	-@erase "$(OUTDIR)\dmdonuts.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /c
CPP_PROJ=/nologo /ML /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS"\
 /Fp"$(INTDIR)/dmdonuts.pch" /YX /Fo"$(INTDIR)/" /c 
CPP_OBJS=.\Release/
CPP_SBRS=.\.
# ADD BASE MTL /nologo /D "NDEBUG" /win32
# ADD MTL /nologo /D "NDEBUG" /win32
MTL_PROJ=/nologo /D "NDEBUG" /win32 
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
RSC_PROJ=/l 0x409 /fo"$(INTDIR)/donuts.res" /d "NDEBUG" 
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/dmdonuts.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib /nologo /subsystem:windows /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib winmm.lib ddraw.lib dinput.lib dsound.lib /nologo /subsystem:windows /machine:I386
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib\
 odbccp32.lib winmm.lib ddraw.lib dinput.lib dsound.lib /nologo\
 /subsystem:windows /incremental:no /pdb:"$(OUTDIR)/dmdonuts.pdb" /machine:I386\
 /out:"$(OUTDIR)/dmdonuts.exe" 
LINK32_OBJS= \
	"$(INTDIR)\ddutil.obj" \
	"$(INTDIR)\donuts.obj" \
	"$(INTDIR)\donuts.res" \
	"$(INTDIR)\dsutil.obj" \
	"$(INTDIR)\input.obj"

"$(OUTDIR)\dmdonuts.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "dmdonuts - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "dmdonuts"
# PROP BASE Intermediate_Dir "dmdonuts"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
OUTDIR=.\Debug
INTDIR=.\Debug

ALL : "$(OUTDIR)\dmdonuts.exe"

CLEAN : 
	-@erase "$(INTDIR)\ddutil.obj"
	-@erase "$(INTDIR)\donuts.obj"
	-@erase "$(INTDIR)\donuts.res"
	-@erase "$(INTDIR)\dsutil.obj"
	-@erase "$(INTDIR)\input.obj"
	-@erase "$(INTDIR)\vc40.idb"
	-@erase "$(INTDIR)\vc40.pdb"
	-@erase "$(OUTDIR)\dmdonuts.exe"
	-@erase "$(OUTDIR)\dmdonuts.ilk"
	-@erase "$(OUTDIR)\dmdonuts.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /c
CPP_PROJ=/nologo /MLd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS"\
 /Fp"$(INTDIR)/dmdonuts.pch" /YX /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c 
CPP_OBJS=.\Debug/
CPP_SBRS=.\.
# ADD BASE MTL /nologo /D "_DEBUG" /win32
# ADD MTL /nologo /D "_DEBUG" /win32
MTL_PROJ=/nologo /D "_DEBUG" /win32 
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
RSC_PROJ=/l 0x409 /fo"$(INTDIR)/donuts.res" /d "_DEBUG" 
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/dmdonuts.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib /nologo /subsystem:windows /debug /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib winmm.lib ddraw.lib dinput.lib dsound.lib /nologo /subsystem:windows /debug /machine:I386
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib\
 odbccp32.lib winmm.lib ddraw.lib dinput.lib dsound.lib /nologo\
 /subsystem:windows /incremental:yes /pdb:"$(OUTDIR)/dmdonuts.pdb" /debug\
 /machine:I386 /out:"$(OUTDIR)/dmdonuts.exe" 
LINK32_OBJS= \
	"$(INTDIR)\ddutil.obj" \
	"$(INTDIR)\donuts.obj" \
	"$(INTDIR)\donuts.res" \
	"$(INTDIR)\dsutil.obj" \
	"$(INTDIR)\input.obj"

"$(OUTDIR)\dmdonuts.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 

.c{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

.cpp{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

.cxx{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

.c{$(CPP_SBRS)}.sbr:
   $(CPP) $(CPP_PROJ) $<  

.cpp{$(CPP_SBRS)}.sbr:
   $(CPP) $(CPP_PROJ) $<  

.cxx{$(CPP_SBRS)}.sbr:
   $(CPP) $(CPP_PROJ) $<  

################################################################################
# Begin Target

# Name "dmdonuts - Win32 Release"
# Name "dmdonuts - Win32 Debug"

!IF  "$(CFG)" == "dmdonuts - Win32 Release"

!ELSEIF  "$(CFG)" == "dmdonuts - Win32 Debug"

!ENDIF 

################################################################################
# Begin Source File

SOURCE=.\donuts.rc
DEP_RSC_DONUT=\
	".\bangbang.wav"\
	".\bounce.wav"\
	".\c_bang.wav"\
	".\d_bang.wav"\
	".\donuts.bmp"\
	".\donuts.ico"\
	".\gunfire.wav"\
	".\hum.wav"\
	".\level.wav"\
	".\p_bang.wav"\
	".\resource.h"\
	".\rev.wav"\
	".\s_bang.wav"\
	".\shield.wav"\
	".\skid.wav"\
	".\splash.bmp"\
	

"$(INTDIR)\donuts.res" : $(SOURCE) $(DEP_RSC_DONUT) "$(INTDIR)"
   $(RSC) $(RSC_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=.\input.cpp
DEP_CPP_INPUT=\
	".\input.h"\
	".\resource.h"\
	{$(INCLUDE)}"\dinput.h"\
	

"$(INTDIR)\input.obj" : $(SOURCE) $(DEP_CPP_INPUT) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\donuts.cpp
DEP_CPP_DONUTS=\
	"..\..\..\..\..\include\dls1.h"\
	"..\..\..\..\..\INCLUDE\dmdls.h"\
	"..\..\..\..\..\INCLUDE\dmerror.h"\
	".\ddutil.h"\
	".\donuts.h"\
	".\dsutil.h"\
	".\input.h"\
	".\resource.h"\
	{$(INCLUDE)}"\dinput.h"\
	{$(INCLUDE)}"\dmusicc.h"\
	{$(INCLUDE)}"\dmusici.h"\
	

"$(INTDIR)\donuts.obj" : $(SOURCE) $(DEP_CPP_DONUTS) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\dsutil.cpp
DEP_CPP_DSUTI=\
	".\dsutil.h"\
	

"$(INTDIR)\dsutil.obj" : $(SOURCE) $(DEP_CPP_DSUTI) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\ddutil.cpp
DEP_CPP_DDUTI=\
	".\ddutil.h"\
	

"$(INTDIR)\ddutil.obj" : $(SOURCE) $(DEP_CPP_DDUTI) "$(INTDIR)"


# End Source File
# End Target
# End Project
################################################################################

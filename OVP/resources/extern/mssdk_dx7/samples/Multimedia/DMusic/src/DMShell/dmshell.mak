# Microsoft Developer Studio Generated NMAKE File, Format Version 4.20
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Application" 0x0101

!IF "$(CFG)" == ""
CFG=dmshell - Win32 Debug
!MESSAGE No configuration specified.  Defaulting to dmshell - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "dmshell - Win32 Release" && "$(CFG)" !=\
 "dmshell - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "dmshell.mak" CFG="dmshell - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "dmshell - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "dmshell - Win32 Debug" (based on "Win32 (x86) Application")
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
RSC=rc.exe
MTL=mktyplib.exe
CPP=cl.exe

!IF  "$(CFG)" == "dmshell - Win32 Release"

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

ALL : "$(OUTDIR)\dmshell.exe"

CLEAN : 
	-@erase "$(INTDIR)\dmplayer.obj"
	-@erase "$(INTDIR)\dmshell.res"
	-@erase "$(INTDIR)\main.obj"
	-@erase "$(INTDIR)\scheme.obj"
	-@erase "$(OUTDIR)\dmshell.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /c
CPP_PROJ=/nologo /ML /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS"\
 /Fp"$(INTDIR)/dmshell.pch" /YX /Fo"$(INTDIR)/" /c 
CPP_OBJS=.\Release/
CPP_SBRS=.\.
# ADD BASE MTL /nologo /D "NDEBUG" /win32
# ADD MTL /nologo /D "NDEBUG" /win32
MTL_PROJ=/nologo /D "NDEBUG" /win32 
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
RSC_PROJ=/l 0x409 /fo"$(INTDIR)/dmshell.res" /d "NDEBUG" 
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/dmshell.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib /nologo /subsystem:windows /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib ole32.lib oleaut32.lib uuid.lib shell32.lib advapi32.lib /nologo /subsystem:windows /machine:I386
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib ole32.lib oleaut32.lib uuid.lib\
 shell32.lib advapi32.lib /nologo /subsystem:windows /incremental:no\
 /pdb:"$(OUTDIR)/dmshell.pdb" /machine:I386 /out:"$(OUTDIR)/dmshell.exe" 
LINK32_OBJS= \
	"$(INTDIR)\dmplayer.obj" \
	"$(INTDIR)\dmshell.res" \
	"$(INTDIR)\main.obj" \
	"$(INTDIR)\scheme.obj"

"$(OUTDIR)\dmshell.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "dmshell - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
OUTDIR=.\Debug
INTDIR=.\Debug

ALL : "$(OUTDIR)\dmshell.exe"

CLEAN : 
	-@erase "$(INTDIR)\dmplayer.obj"
	-@erase "$(INTDIR)\dmshell.res"
	-@erase "$(INTDIR)\main.obj"
	-@erase "$(INTDIR)\scheme.obj"
	-@erase "$(INTDIR)\vc40.idb"
	-@erase "$(INTDIR)\vc40.pdb"
	-@erase "$(OUTDIR)\dmshell.exe"
	-@erase "$(OUTDIR)\dmshell.ilk"
	-@erase "$(OUTDIR)\dmshell.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /c
CPP_PROJ=/nologo /MLd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS"\
 /Fp"$(INTDIR)/dmshell.pch" /YX /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c 
CPP_OBJS=.\Debug/
CPP_SBRS=.\.
# ADD BASE MTL /nologo /D "_DEBUG" /win32
# ADD MTL /nologo /D "_DEBUG" /win32
MTL_PROJ=/nologo /D "_DEBUG" /win32 
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
RSC_PROJ=/l 0x409 /fo"$(INTDIR)/dmshell.res" /d "_DEBUG" 
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/dmshell.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib /nologo /subsystem:windows /debug /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib ole32.lib oleaut32.lib uuid.lib shell32.lib advapi32.lib /nologo /subsystem:windows /debug /machine:I386
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib ole32.lib oleaut32.lib uuid.lib\
 shell32.lib advapi32.lib /nologo /subsystem:windows /incremental:yes\
 /pdb:"$(OUTDIR)/dmshell.pdb" /debug /machine:I386 /out:"$(OUTDIR)/dmshell.exe" 
LINK32_OBJS= \
	"$(INTDIR)\dmplayer.obj" \
	"$(INTDIR)\dmshell.res" \
	"$(INTDIR)\main.obj" \
	"$(INTDIR)\scheme.obj"

"$(OUTDIR)\dmshell.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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

# Name "dmshell - Win32 Release"
# Name "dmshell - Win32 Debug"

!IF  "$(CFG)" == "dmshell - Win32 Release"

!ELSEIF  "$(CFG)" == "dmshell - Win32 Debug"

!ENDIF 

################################################################################
# Begin Source File

SOURCE=.\dmplayer.cpp
DEP_CPP_DMPLA=\
	".\dmplayer.h"\
	".\events.h"\
	".\main.h"\
	".\scheme.h"\
	{$(INCLUDE)}"\dls1.h"\
	{$(INCLUDE)}"\dmusicc.h"\
	{$(INCLUDE)}"\dmusici.h"\
	

"$(INTDIR)\dmplayer.obj" : $(SOURCE) $(DEP_CPP_DMPLA) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\dmplayer.h

!IF  "$(CFG)" == "dmshell - Win32 Release"

!ELSEIF  "$(CFG)" == "dmshell - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\dmshell.rc
DEP_RSC_DMSHE=\
	".\dmplayer.h"\
	".\DMSHELL.ICO"\
	".\events.h"\
	".\main.h"\
	".\scheme.h"\
	{$(INCLUDE)}"\dls1.h"\
	{$(INCLUDE)}"\dmusicc.h"\
	{$(INCLUDE)}"\dmusici.h"\
	

"$(INTDIR)\dmshell.res" : $(SOURCE) $(DEP_RSC_DMSHE) "$(INTDIR)"
   $(RSC) $(RSC_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=.\events.h

!IF  "$(CFG)" == "dmshell - Win32 Release"

!ELSEIF  "$(CFG)" == "dmshell - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\main.cpp
DEP_CPP_MAIN_=\
	".\dmplayer.h"\
	".\events.h"\
	".\main.h"\
	".\scheme.h"\
	{$(INCLUDE)}"\dls1.h"\
	{$(INCLUDE)}"\dmusicc.h"\
	{$(INCLUDE)}"\dmusici.h"\
	

"$(INTDIR)\main.obj" : $(SOURCE) $(DEP_CPP_MAIN_) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\main.h

!IF  "$(CFG)" == "dmshell - Win32 Release"

!ELSEIF  "$(CFG)" == "dmshell - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\resource.h

!IF  "$(CFG)" == "dmshell - Win32 Release"

!ELSEIF  "$(CFG)" == "dmshell - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\scheme.cpp
DEP_CPP_SCHEM=\
	".\dmplayer.h"\
	".\events.h"\
	".\main.h"\
	".\scheme.h"\
	{$(INCLUDE)}"\dls1.h"\
	{$(INCLUDE)}"\dmusicc.h"\
	{$(INCLUDE)}"\dmusici.h"\
	

"$(INTDIR)\scheme.obj" : $(SOURCE) $(DEP_CPP_SCHEM) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\scheme.h

!IF  "$(CFG)" == "dmshell - Win32 Release"

!ELSEIF  "$(CFG)" == "dmshell - Win32 Debug"

!ENDIF 

# End Source File
# End Target
# End Project
################################################################################

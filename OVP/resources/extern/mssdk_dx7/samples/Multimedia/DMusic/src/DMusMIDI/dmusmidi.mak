# Microsoft Developer Studio Generated NMAKE File, Format Version 4.20
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Application" 0x0101

!IF "$(CFG)" == ""
CFG=DMusMIDI - Win32 Debug
!MESSAGE No configuration specified.  Defaulting to DMusMIDI - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "DMusMIDI - Win32 Release" && "$(CFG)" !=\
 "DMusMIDI - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "DMusMIDI.mak" CFG="DMusMIDI - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "DMusMIDI - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "DMusMIDI - Win32 Debug" (based on "Win32 (x86) Application")
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
CPP=cl.exe
RSC=rc.exe
MTL=mktyplib.exe

!IF  "$(CFG)" == "DMusMIDI - Win32 Release"

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

ALL : "$(OUTDIR)\DMusMIDI.exe"

CLEAN : 
	-@erase "$(INTDIR)\Debug.obj"
	-@erase "$(INTDIR)\Mainwnd.obj"
	-@erase "$(INTDIR)\Midiplyr.obj"
	-@erase "$(INTDIR)\Midiplyr.res"
	-@erase "$(INTDIR)\Timewnd.obj"
	-@erase "$(INTDIR)\Uiutils.obj"
	-@erase "$(OUTDIR)\DMusMIDI.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /c
CPP_PROJ=/nologo /ML /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS"\
 /Fp"$(INTDIR)/DMusMIDI.pch" /YX /Fo"$(INTDIR)/" /c 
CPP_OBJS=.\Release/
CPP_SBRS=.\.
# ADD BASE MTL /nologo /D "NDEBUG" /win32
# ADD MTL /nologo /D "NDEBUG" /win32
MTL_PROJ=/nologo /D "NDEBUG" /win32 
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
RSC_PROJ=/l 0x409 /fo"$(INTDIR)/Midiplyr.res" /d "NDEBUG" 
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/DMusMIDI.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /machine:I386
# ADD LINK32 dxguid.lib comctl32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /machine:I386
LINK32_FLAGS=dxguid.lib comctl32.lib kernel32.lib user32.lib gdi32.lib\
 winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib\
 uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /incremental:no\
 /pdb:"$(OUTDIR)/DMusMIDI.pdb" /machine:I386 /out:"$(OUTDIR)/DMusMIDI.exe" 
LINK32_OBJS= \
	"$(INTDIR)\Debug.obj" \
	"$(INTDIR)\Mainwnd.obj" \
	"$(INTDIR)\Midiplyr.obj" \
	"$(INTDIR)\Midiplyr.res" \
	"$(INTDIR)\Timewnd.obj" \
	"$(INTDIR)\Uiutils.obj"

"$(OUTDIR)\DMusMIDI.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "DMusMIDI - Win32 Debug"

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

ALL : "$(OUTDIR)\DMusMIDI.exe"

CLEAN : 
	-@erase "$(INTDIR)\Debug.obj"
	-@erase "$(INTDIR)\Mainwnd.obj"
	-@erase "$(INTDIR)\Midiplyr.obj"
	-@erase "$(INTDIR)\Midiplyr.res"
	-@erase "$(INTDIR)\Timewnd.obj"
	-@erase "$(INTDIR)\Uiutils.obj"
	-@erase "$(INTDIR)\vc40.idb"
	-@erase "$(INTDIR)\vc40.pdb"
	-@erase "$(OUTDIR)\DMusMIDI.exe"
	-@erase "$(OUTDIR)\DMusMIDI.ilk"
	-@erase "$(OUTDIR)\DMusMIDI.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /c
CPP_PROJ=/nologo /MLd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS"\
 /Fp"$(INTDIR)/DMusMIDI.pch" /YX /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c 
CPP_OBJS=.\Debug/
CPP_SBRS=.\.
# ADD BASE MTL /nologo /D "_DEBUG" /win32
# ADD MTL /nologo /D "_DEBUG" /win32
MTL_PROJ=/nologo /D "_DEBUG" /win32 
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
RSC_PROJ=/l 0x409 /fo"$(INTDIR)/Midiplyr.res" /d "_DEBUG" 
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/DMusMIDI.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /debug /machine:I386
# ADD LINK32 dxguid.lib comctl32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /debug /machine:I386
LINK32_FLAGS=dxguid.lib comctl32.lib kernel32.lib user32.lib gdi32.lib\
 winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib\
 uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /incremental:yes\
 /pdb:"$(OUTDIR)/DMusMIDI.pdb" /debug /machine:I386\
 /out:"$(OUTDIR)/DMusMIDI.exe" 
LINK32_OBJS= \
	"$(INTDIR)\Debug.obj" \
	"$(INTDIR)\Mainwnd.obj" \
	"$(INTDIR)\Midiplyr.obj" \
	"$(INTDIR)\Midiplyr.res" \
	"$(INTDIR)\Timewnd.obj" \
	"$(INTDIR)\Uiutils.obj"

"$(OUTDIR)\DMusMIDI.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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

# Name "DMusMIDI - Win32 Release"
# Name "DMusMIDI - Win32 Debug"

!IF  "$(CFG)" == "DMusMIDI - Win32 Release"

!ELSEIF  "$(CFG)" == "DMusMIDI - Win32 Debug"

!ENDIF 

################################################################################
# Begin Source File

SOURCE=.\Uiutils.c
DEP_CPP_UIUTI=\
	".\debug.h"\
	".\global.h"\
	".\midiplyr.h"\
	{$(INCLUDE)}"\dls1.h"\
	{$(INCLUDE)}"\dmdls.h"\
	{$(INCLUDE)}"\dmerror.h"\
	{$(INCLUDE)}"\dmusicc.h"\
	{$(INCLUDE)}"\dmusici.h"\
	

"$(INTDIR)\Uiutils.obj" : $(SOURCE) $(DEP_CPP_UIUTI) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Mainwnd.c
DEP_CPP_MAINW=\
	".\debug.h"\
	".\global.h"\
	".\midiplyr.h"\
	{$(INCLUDE)}"\dls1.h"\
	{$(INCLUDE)}"\dmdls.h"\
	{$(INCLUDE)}"\dmerror.h"\
	{$(INCLUDE)}"\dmksctrl.h"\
	{$(INCLUDE)}"\dmusicc.h"\
	{$(INCLUDE)}"\dmusici.h"\
	

"$(INTDIR)\Mainwnd.obj" : $(SOURCE) $(DEP_CPP_MAINW) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Midiplyr.c
DEP_CPP_MIDIP=\
	".\debug.h"\
	".\global.h"\
	".\midiplyr.h"\
	{$(INCLUDE)}"\dls1.h"\
	{$(INCLUDE)}"\dmdls.h"\
	{$(INCLUDE)}"\dmerror.h"\
	{$(INCLUDE)}"\dmusicc.h"\
	{$(INCLUDE)}"\dmusici.h"\
	

"$(INTDIR)\Midiplyr.obj" : $(SOURCE) $(DEP_CPP_MIDIP) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Midiplyr.rc
DEP_RSC_MIDIPL=\
	".\global.h"\
	".\midiplyr.h"\
	".\MIDIPlyr.ICO"\
	".\TOOLBAR.BMP"\
	{$(INCLUDE)}"\dls1.h"\
	{$(INCLUDE)}"\dmdls.h"\
	{$(INCLUDE)}"\dmerror.h"\
	{$(INCLUDE)}"\dmusicc.h"\
	{$(INCLUDE)}"\dmusici.h"\
	

"$(INTDIR)\Midiplyr.res" : $(SOURCE) $(DEP_RSC_MIDIPL) "$(INTDIR)"
   $(RSC) $(RSC_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Timewnd.c
DEP_CPP_TIMEW=\
	".\debug.h"\
	".\global.h"\
	".\midiplyr.h"\
	{$(INCLUDE)}"\dls1.h"\
	{$(INCLUDE)}"\dmdls.h"\
	{$(INCLUDE)}"\dmerror.h"\
	{$(INCLUDE)}"\dmusicc.h"\
	{$(INCLUDE)}"\dmusici.h"\
	

"$(INTDIR)\Timewnd.obj" : $(SOURCE) $(DEP_CPP_TIMEW) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Debug.c
DEP_CPP_DEBUG=\
	".\debug.h"\
	".\global.h"\
	

"$(INTDIR)\Debug.obj" : $(SOURCE) $(DEP_CPP_DEBUG) "$(INTDIR)"


# End Source File
# End Target
# End Project
################################################################################

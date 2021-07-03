# Microsoft Developer Studio Generated NMAKE File, Format Version 4.20
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Application" 0x0101

!IF "$(CFG)" == ""
CFG=3DMusic - Win32 Debug
!MESSAGE No configuration specified.  Defaulting to 3DMusic - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "3DMusic - Win32 Release" && "$(CFG)" !=\
 "3DMusic - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "3DMusic.mak" CFG="3DMusic - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "3DMusic - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "3DMusic - Win32 Debug" (based on "Win32 (x86) Application")
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
# PROP Target_Last_Scanned "3DMusic - Win32 Debug"
RSC=rc.exe
MTL=mktyplib.exe
CPP=cl.exe

!IF  "$(CFG)" == "3DMusic - Win32 Release"

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

ALL : "$(OUTDIR)\3DMusic.exe"

CLEAN : 
	-@erase "$(INTDIR)\3dsound.obj"
	-@erase "$(INTDIR)\Dsutil3d.obj"
	-@erase "$(INTDIR)\Helper.obj"
	-@erase "$(INTDIR)\Script1.res"
	-@erase "$(INTDIR)\Sound.obj"
	-@erase "$(OUTDIR)\3DMusic.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /c
CPP_PROJ=/nologo /ML /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS"\
 /Fp"$(INTDIR)/3DMusic.pch" /YX /Fo"$(INTDIR)/" /c 
CPP_OBJS=.\Release/
CPP_SBRS=.\.
# ADD BASE MTL /nologo /D "NDEBUG" /win32
# ADD MTL /nologo /D "NDEBUG" /win32
MTL_PROJ=/nologo /D "NDEBUG" /win32 
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
RSC_PROJ=/l 0x409 /fo"$(INTDIR)/Script1.res" /d "NDEBUG" 
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/3DMusic.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib dxguid.lib dsound.lib winmm.lib /nologo /subsystem:windows /machine:I386
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib\
 odbccp32.lib dxguid.lib dsound.lib winmm.lib /nologo /subsystem:windows\
 /incremental:no /pdb:"$(OUTDIR)/3DMusic.pdb" /machine:I386\
 /out:"$(OUTDIR)/3DMusic.exe" 
LINK32_OBJS= \
	"$(INTDIR)\3dsound.obj" \
	"$(INTDIR)\Dsutil3d.obj" \
	"$(INTDIR)\Helper.obj" \
	"$(INTDIR)\Script1.res" \
	"$(INTDIR)\Sound.obj"

"$(OUTDIR)\3DMusic.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "3DMusic - Win32 Debug"

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

ALL : "$(OUTDIR)\3DMusic.exe"

CLEAN : 
	-@erase "$(INTDIR)\3dsound.obj"
	-@erase "$(INTDIR)\Dsutil3d.obj"
	-@erase "$(INTDIR)\Helper.obj"
	-@erase "$(INTDIR)\Script1.res"
	-@erase "$(INTDIR)\Sound.obj"
	-@erase "$(INTDIR)\vc40.idb"
	-@erase "$(INTDIR)\vc40.pdb"
	-@erase "$(OUTDIR)\3DMusic.exe"
	-@erase "$(OUTDIR)\3DMusic.ilk"
	-@erase "$(OUTDIR)\3DMusic.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /c
CPP_PROJ=/nologo /MLd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS"\
 /Fp"$(INTDIR)/3DMusic.pch" /YX /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c 
CPP_OBJS=.\Debug/
CPP_SBRS=.\.
# ADD BASE MTL /nologo /D "_DEBUG" /win32
# ADD MTL /nologo /D "_DEBUG" /win32
MTL_PROJ=/nologo /D "_DEBUG" /win32 
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
RSC_PROJ=/l 0x409 /fo"$(INTDIR)/Script1.res" /d "_DEBUG" 
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/3DMusic.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /debug /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib dxguid.lib dsound.lib winmm.lib /nologo /subsystem:windows /debug /machine:I386
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib\
 odbccp32.lib dxguid.lib dsound.lib winmm.lib /nologo /subsystem:windows\
 /incremental:yes /pdb:"$(OUTDIR)/3DMusic.pdb" /debug /machine:I386\
 /out:"$(OUTDIR)/3DMusic.exe" 
LINK32_OBJS= \
	"$(INTDIR)\3dsound.obj" \
	"$(INTDIR)\Dsutil3d.obj" \
	"$(INTDIR)\Helper.obj" \
	"$(INTDIR)\Script1.res" \
	"$(INTDIR)\Sound.obj"

"$(OUTDIR)\3DMusic.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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

# Name "3DMusic - Win32 Release"
# Name "3DMusic - Win32 Debug"

!IF  "$(CFG)" == "3DMusic - Win32 Release"

!ELSEIF  "$(CFG)" == "3DMusic - Win32 Debug"

!ENDIF 

################################################################################
# Begin Source File

SOURCE=.\Sound.cpp
DEP_CPP_SOUND=\
	"..\..\MSSDK\INCLUDE\dls1.h"\
	"..\..\MSSDK\INCLUDE\dmdls.h"\
	"..\..\MSSDK\INCLUDE\dmerror.h"\
	".\dsutil3d.h"\
	".\helper.h"\
	".\sound.h"\
	{$(INCLUDE)}"\dmusicc.h"\
	{$(INCLUDE)}"\dmusici.h"\
	

"$(INTDIR)\Sound.obj" : $(SOURCE) $(DEP_CPP_SOUND) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Dsutil3d.c
DEP_CPP_DSUTI=\
	".\dsutil3d.h"\
	

"$(INTDIR)\Dsutil3d.obj" : $(SOURCE) $(DEP_CPP_DSUTI) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Helper.cpp
DEP_CPP_HELPE=\
	"..\..\MSSDK\INCLUDE\dls1.h"\
	"..\..\MSSDK\INCLUDE\dmdls.h"\
	"..\..\MSSDK\INCLUDE\dmerror.h"\
	".\helper.h"\
	{$(INCLUDE)}"\dmusicc.h"\
	{$(INCLUDE)}"\dmusici.h"\
	

"$(INTDIR)\Helper.obj" : $(SOURCE) $(DEP_CPP_HELPE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\3dsound.cpp
DEP_CPP_3DSOU=\
	".\sound.h"\
	

"$(INTDIR)\3dsound.obj" : $(SOURCE) $(DEP_CPP_3DSOU) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Script1.rc
DEP_RSC_SCRIP=\
	".\icon1.ico"\
	

"$(INTDIR)\Script1.res" : $(SOURCE) $(DEP_RSC_SCRIP) "$(INTDIR)"
   $(RSC) $(RSC_PROJ) $(SOURCE)


# End Source File
# End Target
# End Project
################################################################################

# Microsoft Developer Studio Generated NMAKE File, Format Version 4.20
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Application" 0x0101

!IF "$(CFG)" == ""
CFG=musiclines - Win32 Debug
!MESSAGE No configuration specified.  Defaulting to musiclines - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "musiclines - Win32 Release" && "$(CFG)" !=\
 "musiclines - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "musiclines.mak" CFG="musiclines - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "musiclines - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "musiclines - Win32 Debug" (based on "Win32 (x86) Application")
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

!IF  "$(CFG)" == "musiclines - Win32 Release"

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

ALL : "$(OUTDIR)\musiclines.exe"

CLEAN : 
	-@erase "$(INTDIR)\debug.obj"
	-@erase "$(INTDIR)\dmhelper.obj"
	-@erase "$(INTDIR)\mlai.obj"
	-@erase "$(INTDIR)\mlbmpsrf.obj"
	-@erase "$(INTDIR)\mlgame.obj"
	-@erase "$(INTDIR)\mlinput.obj"
	-@erase "$(INTDIR)\mllocalplayer.obj"
	-@erase "$(INTDIR)\mlmain.obj"
	-@erase "$(INTDIR)\mlmusic.obj"
	-@erase "$(INTDIR)\mloptpal.obj"
	-@erase "$(INTDIR)\mlrender.obj"
	-@erase "$(INTDIR)\musiclines.res"
	-@erase "$(OUTDIR)\musiclines.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /c
CPP_PROJ=/nologo /ML /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS"\
 /Fp"$(INTDIR)/musiclines.pch" /YX /Fo"$(INTDIR)/" /c 
CPP_OBJS=.\Release/
CPP_SBRS=.\.
# ADD BASE MTL /nologo /D "NDEBUG" /win32
# ADD MTL /nologo /D "NDEBUG" /win32
MTL_PROJ=/nologo /D "NDEBUG" /win32 
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
RSC_PROJ=/l 0x409 /fo"$(INTDIR)/musiclines.res" /d "NDEBUG" 
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/musiclines.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib /nologo /subsystem:windows /machine:I386
# ADD LINK32 dxguid.lib ddraw.lib dinput.lib kernel32.lib user32.lib gdi32.lib ole32.lib advapi32.lib winmm.lib /nologo /subsystem:windows /machine:I386
LINK32_FLAGS=dxguid.lib ddraw.lib dinput.lib kernel32.lib user32.lib gdi32.lib\
 ole32.lib advapi32.lib winmm.lib /nologo /subsystem:windows /incremental:no\
 /pdb:"$(OUTDIR)/musiclines.pdb" /machine:I386 /out:"$(OUTDIR)/musiclines.exe" 
LINK32_OBJS= \
	"$(INTDIR)\debug.obj" \
	"$(INTDIR)\dmhelper.obj" \
	"$(INTDIR)\mlai.obj" \
	"$(INTDIR)\mlbmpsrf.obj" \
	"$(INTDIR)\mlgame.obj" \
	"$(INTDIR)\mlinput.obj" \
	"$(INTDIR)\mllocalplayer.obj" \
	"$(INTDIR)\mlmain.obj" \
	"$(INTDIR)\mlmusic.obj" \
	"$(INTDIR)\mloptpal.obj" \
	"$(INTDIR)\mlrender.obj" \
	"$(INTDIR)\musiclines.res"

"$(OUTDIR)\musiclines.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "musiclines - Win32 Debug"

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

ALL : "$(OUTDIR)\musiclines.exe"

CLEAN : 
	-@erase "$(INTDIR)\debug.obj"
	-@erase "$(INTDIR)\dmhelper.obj"
	-@erase "$(INTDIR)\mlai.obj"
	-@erase "$(INTDIR)\mlbmpsrf.obj"
	-@erase "$(INTDIR)\mlgame.obj"
	-@erase "$(INTDIR)\mlinput.obj"
	-@erase "$(INTDIR)\mllocalplayer.obj"
	-@erase "$(INTDIR)\mlmain.obj"
	-@erase "$(INTDIR)\mlmusic.obj"
	-@erase "$(INTDIR)\mloptpal.obj"
	-@erase "$(INTDIR)\mlrender.obj"
	-@erase "$(INTDIR)\musiclines.res"
	-@erase "$(INTDIR)\vc40.idb"
	-@erase "$(INTDIR)\vc40.pdb"
	-@erase "$(OUTDIR)\musiclines.exe"
	-@erase "$(OUTDIR)\musiclines.ilk"
	-@erase "$(OUTDIR)\musiclines.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /c
CPP_PROJ=/nologo /MLd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS"\
 /Fp"$(INTDIR)/musiclines.pch" /YX /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c 
CPP_OBJS=.\Debug/
CPP_SBRS=.\.
# ADD BASE MTL /nologo /D "_DEBUG" /win32
# ADD MTL /nologo /D "_DEBUG" /win32
MTL_PROJ=/nologo /D "_DEBUG" /win32 
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
RSC_PROJ=/l 0x409 /fo"$(INTDIR)/musiclines.res" /d "_DEBUG" 
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/musiclines.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib /nologo /subsystem:windows /debug /machine:I386
# ADD LINK32 dxguid.lib ddraw.lib dinput.lib kernel32.lib user32.lib gdi32.lib ole32.lib advapi32.lib winmm.lib /nologo /subsystem:windows /debug /machine:I386
LINK32_FLAGS=dxguid.lib ddraw.lib dinput.lib kernel32.lib user32.lib gdi32.lib\
 ole32.lib advapi32.lib winmm.lib /nologo /subsystem:windows /incremental:yes\
 /pdb:"$(OUTDIR)/musiclines.pdb" /debug /machine:I386\
 /out:"$(OUTDIR)/musiclines.exe" 
LINK32_OBJS= \
	"$(INTDIR)\debug.obj" \
	"$(INTDIR)\dmhelper.obj" \
	"$(INTDIR)\mlai.obj" \
	"$(INTDIR)\mlbmpsrf.obj" \
	"$(INTDIR)\mlgame.obj" \
	"$(INTDIR)\mlinput.obj" \
	"$(INTDIR)\mllocalplayer.obj" \
	"$(INTDIR)\mlmain.obj" \
	"$(INTDIR)\mlmusic.obj" \
	"$(INTDIR)\mloptpal.obj" \
	"$(INTDIR)\mlrender.obj" \
	"$(INTDIR)\musiclines.res"

"$(OUTDIR)\musiclines.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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

# Name "musiclines - Win32 Release"
# Name "musiclines - Win32 Debug"

!IF  "$(CFG)" == "musiclines - Win32 Release"

!ELSEIF  "$(CFG)" == "musiclines - Win32 Debug"

!ENDIF 

################################################################################
# Begin Source File

SOURCE=.\debug.cpp

!IF  "$(CFG)" == "musiclines - Win32 Release"

DEP_CPP_DEBUG=\
	".\debug.h"\
	{$(INCLUDE)}"\windows.h"\
	

"$(INTDIR)\debug.obj" : $(SOURCE) $(DEP_CPP_DEBUG) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "musiclines - Win32 Debug"

DEP_CPP_DEBUG=\
	".\debug.h"\
	

"$(INTDIR)\debug.obj" : $(SOURCE) $(DEP_CPP_DEBUG) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\debug.h

!IF  "$(CFG)" == "musiclines - Win32 Release"

!ELSEIF  "$(CFG)" == "musiclines - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\dmhelper.cpp
DEP_CPP_DMHEL=\
	"..\..\..\..\..\INCLUDE\dls1.h"\
	".\debug.h"\
	".\dmhelper.h"\
	{$(INCLUDE)}"\dmusicc.h"\
	{$(INCLUDE)}"\dmusicf.h"\
	{$(INCLUDE)}"\dmusici.h"\
	

"$(INTDIR)\dmhelper.obj" : $(SOURCE) $(DEP_CPP_DMHEL) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\dmhelper.h

!IF  "$(CFG)" == "musiclines - Win32 Release"

!ELSEIF  "$(CFG)" == "musiclines - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\mlai.cpp
DEP_CPP_MLAI_=\
	".\debug.h"\
	".\mlai.h"\
	".\mlgame.h"\
	".\musiclines.h"\
	

"$(INTDIR)\mlai.obj" : $(SOURCE) $(DEP_CPP_MLAI_) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\mlai.h

!IF  "$(CFG)" == "musiclines - Win32 Release"

!ELSEIF  "$(CFG)" == "musiclines - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\mlbmpsrf.cpp
DEP_CPP_MLBMP=\
	".\debug.h"\
	".\mlbmpsrf.h"\
	".\mloptpal.h"\
	

"$(INTDIR)\mlbmpsrf.obj" : $(SOURCE) $(DEP_CPP_MLBMP) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\mlbmpsrf.h

!IF  "$(CFG)" == "musiclines - Win32 Release"

!ELSEIF  "$(CFG)" == "musiclines - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\mlgame.cpp
DEP_CPP_MLGAM=\
	"..\..\..\..\..\INCLUDE\dls1.h"\
	".\debug.h"\
	".\mlai.h"\
	".\mlgame.h"\
	".\mlinput.h"\
	".\mllocalplayer.h"\
	".\mlmusic.h"\
	".\mlrender.h"\
	".\musiclines.h"\
	".\resource.h"\
	{$(INCLUDE)}"\dinput.h"\
	{$(INCLUDE)}"\dmusicc.h"\
	{$(INCLUDE)}"\dmusici.h"\
	

"$(INTDIR)\mlgame.obj" : $(SOURCE) $(DEP_CPP_MLGAM) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\mlgame.h

!IF  "$(CFG)" == "musiclines - Win32 Release"

!ELSEIF  "$(CFG)" == "musiclines - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\mlinput.cpp
DEP_CPP_MLINP=\
	".\debug.h"\
	".\mlinput.h"\
	{$(INCLUDE)}"\dinput.h"\
	

"$(INTDIR)\mlinput.obj" : $(SOURCE) $(DEP_CPP_MLINP) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\mlinput.h

!IF  "$(CFG)" == "musiclines - Win32 Release"

!ELSEIF  "$(CFG)" == "musiclines - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\mllocalplayer.cpp
DEP_CPP_MLLOC=\
	"..\..\..\..\..\INCLUDE\dls1.h"\
	".\debug.h"\
	".\mlgame.h"\
	".\mlinput.h"\
	".\mllocalplayer.h"\
	".\mlmusic.h"\
	".\musiclines.h"\
	{$(INCLUDE)}"\dinput.h"\
	{$(INCLUDE)}"\dmusicc.h"\
	{$(INCLUDE)}"\dmusici.h"\
	

"$(INTDIR)\mllocalplayer.obj" : $(SOURCE) $(DEP_CPP_MLLOC) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\mllocalplayer.h

!IF  "$(CFG)" == "musiclines - Win32 Release"

!ELSEIF  "$(CFG)" == "musiclines - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\mlmain.cpp
DEP_CPP_MLMAI=\
	"..\..\..\..\..\INCLUDE\dls1.h"\
	".\debug.h"\
	".\mlgame.h"\
	".\mlinput.h"\
	".\mlmusic.h"\
	".\mlrender.h"\
	".\musiclines.h"\
	".\resource.h"\
	{$(INCLUDE)}"\dinput.h"\
	{$(INCLUDE)}"\dmusicc.h"\
	{$(INCLUDE)}"\dmusici.h"\
	

"$(INTDIR)\mlmain.obj" : $(SOURCE) $(DEP_CPP_MLMAI) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\mlmusic.cpp
DEP_CPP_MLMUS=\
	"..\..\..\..\..\INCLUDE\dls1.h"\
	".\debug.h"\
	".\dmhelper.h"\
	".\mlmusic.h"\
	{$(INCLUDE)}"\dmusicc.h"\
	{$(INCLUDE)}"\dmusici.h"\
	

"$(INTDIR)\mlmusic.obj" : $(SOURCE) $(DEP_CPP_MLMUS) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\mlmusic.h

!IF  "$(CFG)" == "musiclines - Win32 Release"

!ELSEIF  "$(CFG)" == "musiclines - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\mloptpal.cpp
DEP_CPP_MLOPT=\
	".\debug.h"\
	".\mloptpal.h"\
	

"$(INTDIR)\mloptpal.obj" : $(SOURCE) $(DEP_CPP_MLOPT) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\mloptpal.h

!IF  "$(CFG)" == "musiclines - Win32 Release"

!ELSEIF  "$(CFG)" == "musiclines - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\mlrender.cpp
DEP_CPP_MLREN=\
	".\debug.h"\
	".\mlbmpsrf.h"\
	".\mlgame.h"\
	".\mloptpal.h"\
	".\mlrender.h"\
	".\musiclines.h"\
	

"$(INTDIR)\mlrender.obj" : $(SOURCE) $(DEP_CPP_MLREN) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\mlrender.h

!IF  "$(CFG)" == "musiclines - Win32 Release"

!ELSEIF  "$(CFG)" == "musiclines - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\musiclines.h

!IF  "$(CFG)" == "musiclines - Win32 Release"

!ELSEIF  "$(CFG)" == "musiclines - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\musiclines.rc
DEP_RSC_MUSIC=\
	".\BACKDROP2.BMP"\
	".\PLAYER0.BMP"\
	".\PLAYER1.BMP"\
	".\resource.h"\
	".\SCORE0.BMP"\
	".\SCORE1.BMP"\
	

"$(INTDIR)\musiclines.res" : $(SOURCE) $(DEP_RSC_MUSIC) "$(INTDIR)"
   $(RSC) $(RSC_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=.\resource.h

!IF  "$(CFG)" == "musiclines - Win32 Release"

!ELSEIF  "$(CFG)" == "musiclines - Win32 Debug"

!ENDIF 

# End Source File
# End Target
# End Project
################################################################################

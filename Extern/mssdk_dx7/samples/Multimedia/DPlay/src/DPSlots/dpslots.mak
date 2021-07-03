# Microsoft Developer Studio Generated NMAKE File, Format Version 4.20
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Application" 0x0101

!IF "$(CFG)" == ""
CFG=dpslots - Win32 Debug
!MESSAGE No configuration specified.  Defaulting to dpslots - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "dpslots - Win32 Release" && "$(CFG)" !=\
 "dpslots - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "dpslots.mak" CFG="dpslots - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "dpslots - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "dpslots - Win32 Debug" (based on "Win32 (x86) Application")
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

!IF  "$(CFG)" == "dpslots - Win32 Release"

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

ALL : "$(OUTDIR)\dpslots.exe"

CLEAN : 
	-@erase "$(INTDIR)\client.obj"
	-@erase "$(INTDIR)\dialog.obj"
	-@erase "$(INTDIR)\dpslots.obj"
	-@erase "$(INTDIR)\dpslots.res"
	-@erase "$(INTDIR)\lobby.obj"
	-@erase "$(INTDIR)\server.obj"
	-@erase "$(OUTDIR)\dpslots.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /c
CPP_PROJ=/nologo /ML /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS"\
 /Fp"$(INTDIR)/dpslots.pch" /YX /Fo"$(INTDIR)/" /c 
CPP_OBJS=.\Release/
CPP_SBRS=.\.
# ADD BASE MTL /nologo /D "NDEBUG" /win32
# ADD MTL /nologo /D "NDEBUG" /win32
MTL_PROJ=/nologo /D "NDEBUG" /win32 
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
RSC_PROJ=/l 0x409 /fo"$(INTDIR)/dpslots.res" /d "NDEBUG" 
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/dpslots.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib /nologo /subsystem:windows /machine:I386
# ADD LINK32 dplayx.lib winmm.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib /nologo /subsystem:windows /machine:I386
LINK32_FLAGS=dplayx.lib winmm.lib kernel32.lib user32.lib gdi32.lib\
 winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib\
 uuid.lib /nologo /subsystem:windows /incremental:no\
 /pdb:"$(OUTDIR)/dpslots.pdb" /machine:I386 /out:"$(OUTDIR)/dpslots.exe" 
LINK32_OBJS= \
	"$(INTDIR)\client.obj" \
	"$(INTDIR)\dialog.obj" \
	"$(INTDIR)\dpslots.obj" \
	"$(INTDIR)\dpslots.res" \
	"$(INTDIR)\lobby.obj" \
	"$(INTDIR)\server.obj"

"$(OUTDIR)\dpslots.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "dpslots - Win32 Debug"

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

ALL : "$(OUTDIR)\dpslots.exe"

CLEAN : 
	-@erase "$(INTDIR)\client.obj"
	-@erase "$(INTDIR)\dialog.obj"
	-@erase "$(INTDIR)\dpslots.obj"
	-@erase "$(INTDIR)\dpslots.res"
	-@erase "$(INTDIR)\lobby.obj"
	-@erase "$(INTDIR)\server.obj"
	-@erase "$(INTDIR)\vc40.idb"
	-@erase "$(INTDIR)\vc40.pdb"
	-@erase "$(OUTDIR)\dpslots.exe"
	-@erase "$(OUTDIR)\dpslots.ilk"
	-@erase "$(OUTDIR)\dpslots.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /c
CPP_PROJ=/nologo /MLd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS"\
 /Fp"$(INTDIR)/dpslots.pch" /YX /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c 
CPP_OBJS=.\Debug/
CPP_SBRS=.\.
# ADD BASE MTL /nologo /D "_DEBUG" /win32
# ADD MTL /nologo /D "_DEBUG" /win32
MTL_PROJ=/nologo /D "_DEBUG" /win32 
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
RSC_PROJ=/l 0x409 /fo"$(INTDIR)/dpslots.res" /d "_DEBUG" 
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/dpslots.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib /nologo /subsystem:windows /debug /machine:I386
# ADD LINK32 dplayx.lib winmm.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib /nologo /subsystem:windows /debug /machine:I386
LINK32_FLAGS=dplayx.lib winmm.lib kernel32.lib user32.lib gdi32.lib\
 winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib\
 uuid.lib /nologo /subsystem:windows /incremental:yes\
 /pdb:"$(OUTDIR)/dpslots.pdb" /debug /machine:I386 /out:"$(OUTDIR)/dpslots.exe" 
LINK32_OBJS= \
	"$(INTDIR)\client.obj" \
	"$(INTDIR)\dialog.obj" \
	"$(INTDIR)\dpslots.obj" \
	"$(INTDIR)\dpslots.res" \
	"$(INTDIR)\lobby.obj" \
	"$(INTDIR)\server.obj"

"$(OUTDIR)\dpslots.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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

# Name "dpslots - Win32 Release"
# Name "dpslots - Win32 Debug"

!IF  "$(CFG)" == "dpslots - Win32 Release"

!ELSEIF  "$(CFG)" == "dpslots - Win32 Debug"

!ENDIF 

################################################################################
# Begin Source File

SOURCE=.\lobby.cpp
DEP_CPP_LOBBY=\
	".\dpslots.h"\
	{$(INCLUDE)}"\dplobby.h"\
	

"$(INTDIR)\lobby.obj" : $(SOURCE) $(DEP_CPP_LOBBY) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\dialog.cpp
DEP_CPP_DIALO=\
	".\dpslots.h"\
	{$(INCLUDE)}"\dplobby.h"\
	

"$(INTDIR)\dialog.obj" : $(SOURCE) $(DEP_CPP_DIALO) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\dpslots.cpp
DEP_CPP_DPSLO=\
	".\dpslots.h"\
	{$(INCLUDE)}"\dplobby.h"\
	

"$(INTDIR)\dpslots.obj" : $(SOURCE) $(DEP_CPP_DPSLO) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\dpslots.h

!IF  "$(CFG)" == "dpslots - Win32 Release"

!ELSEIF  "$(CFG)" == "dpslots - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\dpslots.rc
DEP_RSC_DPSLOT=\
	".\media\ding.wav"\
	".\media\lose.wav"\
	".\media\wheel.bmp"\
	".\media\win.wav"\
	

"$(INTDIR)\dpslots.res" : $(SOURCE) $(DEP_RSC_DPSLOT) "$(INTDIR)"
   $(RSC) $(RSC_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=.\client.cpp
DEP_CPP_CLIEN=\
	".\dpslots.h"\
	{$(INCLUDE)}"\dplobby.h"\
	

"$(INTDIR)\client.obj" : $(SOURCE) $(DEP_CPP_CLIEN) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\resource.h

!IF  "$(CFG)" == "dpslots - Win32 Release"

!ELSEIF  "$(CFG)" == "dpslots - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\server.cpp
DEP_CPP_SERVE=\
	".\dpslots.h"\
	{$(INCLUDE)}"\dplobby.h"\
	

"$(INTDIR)\server.obj" : $(SOURCE) $(DEP_CPP_SERVE) "$(INTDIR)"


# End Source File
# End Target
# End Project
################################################################################

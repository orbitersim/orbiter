# Microsoft Developer Studio Generated NMAKE File, Format Version 4.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Application" 0x0101

!IF "$(CFG)" == ""
CFG=duel - Win32 Debug
!MESSAGE No configuration specified.  Defaulting to duel - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "duel - Win32 Release" && "$(CFG)" != "duel - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "duel.mak" CFG="duel - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "duel - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "duel - Win32 Debug" (based on "Win32 (x86) Application")
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
# PROP Target_Last_Scanned "duel - Win32 Debug"
CPP=cl.exe
MTL=mktyplib.exe
RSC=rc.exe

!IF  "$(CFG)" == "duel - Win32 Release"

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

ALL : "$(OUTDIR)\duel.exe"

CLEAN : 
	-@erase ".\Release\duel.exe"
	-@erase ".\Release\dsutil.obj"
	-@erase ".\Release\gfx.obj"
	-@erase ".\Release\diutil.obj"
	-@erase ".\Release\lobby.obj"
	-@erase ".\Release\duel.obj"
	-@erase ".\Release\ddutil.obj"
	-@erase ".\Release\dputil.obj"
	-@erase ".\Release\gameproc.obj"
	-@erase ".\Release\util.obj"
	-@erase ".\Release\duel.res"
	-@erase ".\Release\dpconnect.obj"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /W3 /GX /Z7 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /c
# SUBTRACT CPP /O<none>
CPP_PROJ=/nologo /ML /W3 /GX /Z7 /D "WIN32" /D "NDEBUG" /D "_WINDOWS"\
 /Fp"$(INTDIR)/duel.pch" /YX /Fo"$(INTDIR)/" /c 
CPP_OBJS=.\Release/
CPP_SBRS=
# ADD BASE MTL /nologo /D "NDEBUG" /win32
# ADD MTL /nologo /D "NDEBUG" /win32
MTL_PROJ=/nologo /D "NDEBUG" /win32 
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
RSC_PROJ=/l 0x409 /fo"$(INTDIR)/duel.res" /d "NDEBUG" 
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/duel.bsc" 
BSC32_SBRS=
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib /nologo /subsystem:windows /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib ole32.lib comdlg32.lib advapi32.lib shell32.lib uuid.lib comctl32.lib winmm.lib dplayx.lib ddraw.lib dinput.lib dsound.lib /nologo /subsystem:windows /machine:I386
# SUBTRACT LINK32 /nodefaultlib
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib ole32.lib comdlg32.lib\
 advapi32.lib shell32.lib uuid.lib comctl32.lib winmm.lib dplayx.lib ddraw.lib\
 dinput.lib dsound.lib /nologo /subsystem:windows /incremental:no\
 /pdb:"$(OUTDIR)/duel.pdb" /machine:I386 /out:"$(OUTDIR)/duel.exe" 
LINK32_OBJS= \
	"$(INTDIR)/dsutil.obj" \
	"$(INTDIR)/gfx.obj" \
	"$(INTDIR)/diutil.obj" \
	"$(INTDIR)/lobby.obj" \
	"$(INTDIR)/duel.obj" \
	"$(INTDIR)/ddutil.obj" \
	"$(INTDIR)/dputil.obj" \
	"$(INTDIR)/gameproc.obj" \
	"$(INTDIR)/util.obj" \
	"$(INTDIR)/dpconnect.obj" \
	"$(INTDIR)/duel.res"

"$(OUTDIR)\duel.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "duel - Win32 Debug"

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

ALL : "$(OUTDIR)\duel.exe" "$(OUTDIR)\duel.bsc"

CLEAN : 
	-@erase ".\Debug\duel.bsc"
	-@erase ".\Debug\duel.sbr"
	-@erase ".\Debug\ddutil.sbr"
	-@erase ".\Debug\dputil.sbr"
	-@erase ".\Debug\gameproc.sbr"
	-@erase ".\Debug\gfx.sbr"
	-@erase ".\Debug\dsutil.sbr"
	-@erase ".\Debug\lobby.sbr"
	-@erase ".\Debug\util.sbr"
	-@erase ".\Debug\diutil.sbr"
	-@erase ".\Debug\dpconnect.sbr"
	-@erase ".\Debug\duel.exe"
	-@erase ".\Debug\lobby.obj"
	-@erase ".\Debug\util.obj"
	-@erase ".\Debug\diutil.obj"
	-@erase ".\Debug\duel.obj"
	-@erase ".\Debug\ddutil.obj"
	-@erase ".\Debug\dputil.obj"
	-@erase ".\Debug\gameproc.obj"
	-@erase ".\Debug\gfx.obj"
	-@erase ".\Debug\dsutil.obj"
	-@erase ".\Debug\duel.res"
	-@erase ".\Debug\dpconnect.obj"
	-@erase ".\Debug\duel.ilk"
	-@erase ".\Debug\duel.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /W3 /GX /Z7 /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /FR /YX /c
# SUBTRACT CPP /O<none>
CPP_PROJ=/nologo /MLd /W3 /GX /Z7 /D "WIN32" /D "_DEBUG" /D "_WINDOWS"\
 /FR"$(INTDIR)/" /Fp"$(INTDIR)/duel.pch" /YX /Fo"$(INTDIR)/" /c 
CPP_OBJS=.\Debug/
CPP_SBRS=.\Debug/
# ADD BASE MTL /nologo /D "_DEBUG" /win32
# ADD MTL /nologo /D "_DEBUG" /win32
MTL_PROJ=/nologo /D "_DEBUG" /win32 
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
RSC_PROJ=/l 0x409 /fo"$(INTDIR)/duel.res" /d "_DEBUG" 
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/duel.bsc" 
BSC32_SBRS= \
	"$(INTDIR)/duel.sbr" \
	"$(INTDIR)/ddutil.sbr" \
	"$(INTDIR)/dputil.sbr" \
	"$(INTDIR)/gameproc.sbr" \
	"$(INTDIR)/gfx.sbr" \
	"$(INTDIR)/dsutil.sbr" \
	"$(INTDIR)/lobby.sbr" \
	"$(INTDIR)/util.sbr" \
	"$(INTDIR)/diutil.sbr" \
	"$(INTDIR)/dpconnect.sbr"

"$(OUTDIR)\duel.bsc" : "$(OUTDIR)" $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib /nologo /subsystem:windows /debug /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib ole32.lib comdlg32.lib advapi32.lib shell32.lib uuid.lib comctl32.lib winmm.lib dplayx.lib ddraw.lib dinput.lib dsound.lib /nologo /subsystem:windows /debug /machine:I386
# SUBTRACT LINK32 /nodefaultlib
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib ole32.lib comdlg32.lib\
 advapi32.lib shell32.lib uuid.lib comctl32.lib winmm.lib dplayx.lib ddraw.lib\
 dinput.lib dsound.lib /nologo /subsystem:windows /incremental:yes\
 /pdb:"$(OUTDIR)/duel.pdb" /debug /machine:I386 /out:"$(OUTDIR)/duel.exe" 
LINK32_OBJS= \
	"$(INTDIR)/lobby.obj" \
	"$(INTDIR)/util.obj" \
	"$(INTDIR)/diutil.obj" \
	"$(INTDIR)/duel.obj" \
	"$(INTDIR)/ddutil.obj" \
	"$(INTDIR)/dputil.obj" \
	"$(INTDIR)/gameproc.obj" \
	"$(INTDIR)/gfx.obj" \
	"$(INTDIR)/dsutil.obj" \
	"$(INTDIR)/dpconnect.obj" \
	"$(INTDIR)/duel.res"

"$(OUTDIR)\duel.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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

# Name "duel - Win32 Release"
# Name "duel - Win32 Debug"

!IF  "$(CFG)" == "duel - Win32 Release"

!ELSEIF  "$(CFG)" == "duel - Win32 Debug"

!ENDIF 

################################################################################
# Begin Source File

SOURCE=.\ddutil.h

!IF  "$(CFG)" == "duel - Win32 Release"

!ELSEIF  "$(CFG)" == "duel - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\diutil.cpp
DEP_CPP_DIUTI=\
	".\duel.h"\
	".\diutil.h"\
	".\gameproc.h"\
	{$(INCLUDE)}"\dinput.h"\
	{$(INCLUDE)}"\ddraw.h"\
	{$(INCLUDE)}"\dplay.h"\
	{$(INCLUDE)}"\dsound.h"\
	"..\..\..\..\..\INCLUDE\d3dtypes.h"\
	"..\..\..\..\..\INCLUDE\d3dvec.inl"\
	

!IF  "$(CFG)" == "duel - Win32 Release"


"$(INTDIR)\diutil.obj" : $(SOURCE) $(DEP_CPP_DIUTI) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "duel - Win32 Debug"


"$(INTDIR)\diutil.obj" : $(SOURCE) $(DEP_CPP_DIUTI) "$(INTDIR)"

"$(INTDIR)\diutil.sbr" : $(SOURCE) $(DEP_CPP_DIUTI) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\diutil.h

!IF  "$(CFG)" == "duel - Win32 Release"

!ELSEIF  "$(CFG)" == "duel - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\dputil.cpp
DEP_CPP_DPUTI=\
	".\duel.h"\
	".\dputil.h"\
	".\lobby.h"\
	{$(INCLUDE)}"\dplay.h"\
	{$(INCLUDE)}"\dplobby.h"\
	

!IF  "$(CFG)" == "duel - Win32 Release"


"$(INTDIR)\dputil.obj" : $(SOURCE) $(DEP_CPP_DPUTI) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "duel - Win32 Debug"


"$(INTDIR)\dputil.obj" : $(SOURCE) $(DEP_CPP_DPUTI) "$(INTDIR)"

"$(INTDIR)\dputil.sbr" : $(SOURCE) $(DEP_CPP_DPUTI) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\dputil.h

!IF  "$(CFG)" == "duel - Win32 Release"

!ELSEIF  "$(CFG)" == "duel - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\dsutil.cpp
DEP_CPP_DSUTI=\
	{$(INCLUDE)}"\dsound.h"\
	".\dsutil.h"\
	"..\..\..\..\..\INCLUDE\d3dtypes.h"\
	{$(INCLUDE)}"\ddraw.h"\
	"..\..\..\..\..\INCLUDE\d3dvec.inl"\
	

!IF  "$(CFG)" == "duel - Win32 Release"


"$(INTDIR)\dsutil.obj" : $(SOURCE) $(DEP_CPP_DSUTI) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "duel - Win32 Debug"


"$(INTDIR)\dsutil.obj" : $(SOURCE) $(DEP_CPP_DSUTI) "$(INTDIR)"

"$(INTDIR)\dsutil.sbr" : $(SOURCE) $(DEP_CPP_DSUTI) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\dsutil.h

!IF  "$(CFG)" == "duel - Win32 Release"

!ELSEIF  "$(CFG)" == "duel - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\duel.cpp
DEP_CPP_DUEL_=\
	".\duel.h"\
	".\gameproc.h"\
	".\gfx.h"\
	".\dputil.h"\
	".\diutil.h"\
	".\dsutil.h"\
	".\lobby.h"\
	{$(INCLUDE)}"\ddraw.h"\
	{$(INCLUDE)}"\dplay.h"\
	{$(INCLUDE)}"\dsound.h"\
	"..\..\..\..\..\INCLUDE\d3dtypes.h"\
	"..\..\..\..\..\INCLUDE\d3dvec.inl"\
	{$(INCLUDE)}"\dinput.h"\
	{$(INCLUDE)}"\dplobby.h"\
	

!IF  "$(CFG)" == "duel - Win32 Release"


"$(INTDIR)\duel.obj" : $(SOURCE) $(DEP_CPP_DUEL_) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "duel - Win32 Debug"


"$(INTDIR)\duel.obj" : $(SOURCE) $(DEP_CPP_DUEL_) "$(INTDIR)"

"$(INTDIR)\duel.sbr" : $(SOURCE) $(DEP_CPP_DUEL_) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\duel.h

!IF  "$(CFG)" == "duel - Win32 Release"

!ELSEIF  "$(CFG)" == "duel - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\duel.rc
DEP_RSC_DUEL_R=\
	".\Bfire.wav"\
	".\csession.bmp"\
	".\DUEL.BMP"\
	".\duel.ico"\
	".\Lboom.wav"\
	".\osession.bmp"\
	".\player.bmp"\
	".\Sboom.wav"\
	".\Sbounce.wav"\
	".\Sengine.wav"\
	".\SPLASH.BMP"\
	".\Sstart.wav"\
	".\Sstop.wav"\
	".\verinfo.h"\
	".\verinfo.ver"\
	

"$(INTDIR)\duel.res" : $(SOURCE) $(DEP_RSC_DUEL_R) "$(INTDIR)"
   $(RSC) $(RSC_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=.\gameproc.cpp
DEP_CPP_GAMEP=\
	".\duel.h"\
	".\gameproc.h"\
	".\gfx.h"\
	".\dputil.h"\
	".\diutil.h"\
	".\dsutil.h"\
	".\lobby.h"\
	{$(INCLUDE)}"\dsound.h"\
	{$(INCLUDE)}"\ddraw.h"\
	{$(INCLUDE)}"\dplay.h"\
	{$(INCLUDE)}"\dinput.h"\
	{$(INCLUDE)}"\dplobby.h"\
	"..\..\..\..\..\INCLUDE\d3dtypes.h"\
	"..\..\..\..\..\INCLUDE\d3dvec.inl"\
	

!IF  "$(CFG)" == "duel - Win32 Release"


"$(INTDIR)\gameproc.obj" : $(SOURCE) $(DEP_CPP_GAMEP) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "duel - Win32 Debug"


"$(INTDIR)\gameproc.obj" : $(SOURCE) $(DEP_CPP_GAMEP) "$(INTDIR)"

"$(INTDIR)\gameproc.sbr" : $(SOURCE) $(DEP_CPP_GAMEP) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\gameproc.h

!IF  "$(CFG)" == "duel - Win32 Release"

!ELSEIF  "$(CFG)" == "duel - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\gfx.cpp
DEP_CPP_GFX_C=\
	".\duel.h"\
	".\gfx.h"\
	".\diutil.h"\
	".\ddutil.h"\
	{$(INCLUDE)}"\ddraw.h"\
	{$(INCLUDE)}"\dinput.h"\
	

!IF  "$(CFG)" == "duel - Win32 Release"


"$(INTDIR)\gfx.obj" : $(SOURCE) $(DEP_CPP_GFX_C) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "duel - Win32 Debug"


"$(INTDIR)\gfx.obj" : $(SOURCE) $(DEP_CPP_GFX_C) "$(INTDIR)"

"$(INTDIR)\gfx.sbr" : $(SOURCE) $(DEP_CPP_GFX_C) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\gfx.h

!IF  "$(CFG)" == "duel - Win32 Release"

!ELSEIF  "$(CFG)" == "duel - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\lobby.cpp
DEP_CPP_LOBBY=\
	".\duel.h"\
	".\lobby.h"\
	{$(INCLUDE)}"\dplobby.h"\
	{$(INCLUDE)}"\dplay.h"\
	

!IF  "$(CFG)" == "duel - Win32 Release"


"$(INTDIR)\lobby.obj" : $(SOURCE) $(DEP_CPP_LOBBY) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "duel - Win32 Debug"


"$(INTDIR)\lobby.obj" : $(SOURCE) $(DEP_CPP_LOBBY) "$(INTDIR)"

"$(INTDIR)\lobby.sbr" : $(SOURCE) $(DEP_CPP_LOBBY) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\lobby.h

!IF  "$(CFG)" == "duel - Win32 Release"

!ELSEIF  "$(CFG)" == "duel - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\resource.h

!IF  "$(CFG)" == "duel - Win32 Release"

!ELSEIF  "$(CFG)" == "duel - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\util.cpp
DEP_CPP_UTIL_=\
	".\duel.h"\
	

!IF  "$(CFG)" == "duel - Win32 Release"


"$(INTDIR)\util.obj" : $(SOURCE) $(DEP_CPP_UTIL_) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "duel - Win32 Debug"


"$(INTDIR)\util.obj" : $(SOURCE) $(DEP_CPP_UTIL_) "$(INTDIR)"

"$(INTDIR)\util.sbr" : $(SOURCE) $(DEP_CPP_UTIL_) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\ddutil.cpp
DEP_CPP_DDUTI=\
	{$(INCLUDE)}"\ddraw.h"\
	".\ddutil.h"\
	

!IF  "$(CFG)" == "duel - Win32 Release"


"$(INTDIR)\ddutil.obj" : $(SOURCE) $(DEP_CPP_DDUTI) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "duel - Win32 Debug"


"$(INTDIR)\ddutil.obj" : $(SOURCE) $(DEP_CPP_DDUTI) "$(INTDIR)"

"$(INTDIR)\ddutil.sbr" : $(SOURCE) $(DEP_CPP_DDUTI) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\dpconnect.cpp
DEP_CPP_DPCON=\
	{$(INCLUDE)}"\dplobby.h"\
	{$(INCLUDE)}"\dplay.h"\
	

!IF  "$(CFG)" == "duel - Win32 Release"


"$(INTDIR)\dpconnect.obj" : $(SOURCE) $(DEP_CPP_DPCON) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "duel - Win32 Debug"


"$(INTDIR)\dpconnect.obj" : $(SOURCE) $(DEP_CPP_DPCON) "$(INTDIR)"

"$(INTDIR)\dpconnect.sbr" : $(SOURCE) $(DEP_CPP_DPCON) "$(INTDIR)"


!ENDIF 

# End Source File
# End Target
# End Project
################################################################################

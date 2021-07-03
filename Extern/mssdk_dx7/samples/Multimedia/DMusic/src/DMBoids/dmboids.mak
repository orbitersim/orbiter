# Microsoft Developer Studio Generated NMAKE File, Format Version 4.20
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Application" 0x0101

!IF "$(CFG)" == ""
CFG=dmboids - Win32 Debug
!MESSAGE No configuration specified.  Defaulting to dmboids - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "dmboids - Win32 Release" && "$(CFG)" !=\
 "dmboids - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "dmboids.mak" CFG="dmboids - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "dmboids - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "dmboids - Win32 Debug" (based on "Win32 (x86) Application")
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
# PROP Target_Last_Scanned "dmboids - Win32 Debug"
RSC=rc.exe
MTL=mktyplib.exe
CPP=cl.exe

!IF  "$(CFG)" == "dmboids - Win32 Release"

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

ALL : "$(OUTDIR)\dmboids.exe"

CLEAN : 
	-@erase "$(INTDIR)\boids.obj"
	-@erase "$(INTDIR)\d3dtex.obj"
	-@erase "$(INTDIR)\d3dutils.obj"
	-@erase "$(INTDIR)\d3dwin.obj"
	-@erase "$(INTDIR)\debug.obj"
	-@erase "$(INTDIR)\dmboids.res"
	-@erase "$(INTDIR)\drvmgr.obj"
	-@erase "$(INTDIR)\flock.obj"
	-@erase "$(INTDIR)\gull.obj"
	-@erase "$(INTDIR)\input.obj"
	-@erase "$(INTDIR)\music.obj"
	-@erase "$(INTDIR)\winmain.obj"
	-@erase "$(INTDIR)\winproc.obj"
	-@erase "$(OUTDIR)\dmboids.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /c
CPP_PROJ=/nologo /ML /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS"\
 /Fp"$(INTDIR)/dmboids.pch" /YX /Fo"$(INTDIR)/" /c 
CPP_OBJS=.\Release/
CPP_SBRS=.\.
# ADD BASE MTL /nologo /D "NDEBUG" /win32
# ADD MTL /nologo /D "NDEBUG" /win32
MTL_PROJ=/nologo /D "NDEBUG" /win32 
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
RSC_PROJ=/l 0x409 /fo"$(INTDIR)/dmboids.res" /d "NDEBUG" 
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/dmboids.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib /nologo /subsystem:windows /machine:I386
# ADD LINK32 ddraw.lib dsound.lib dinput.lib dxguid.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib /nologo /subsystem:windows /machine:I386
LINK32_FLAGS=ddraw.lib dsound.lib dinput.lib dxguid.lib kernel32.lib user32.lib\
 gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib\
 oleaut32.lib uuid.lib /nologo /subsystem:windows /incremental:no\
 /pdb:"$(OUTDIR)/dmboids.pdb" /machine:I386 /out:"$(OUTDIR)/dmboids.exe" 
LINK32_OBJS= \
	"$(INTDIR)\boids.obj" \
	"$(INTDIR)\d3dtex.obj" \
	"$(INTDIR)\d3dutils.obj" \
	"$(INTDIR)\d3dwin.obj" \
	"$(INTDIR)\debug.obj" \
	"$(INTDIR)\dmboids.res" \
	"$(INTDIR)\drvmgr.obj" \
	"$(INTDIR)\flock.obj" \
	"$(INTDIR)\gull.obj" \
	"$(INTDIR)\input.obj" \
	"$(INTDIR)\music.obj" \
	"$(INTDIR)\winmain.obj" \
	"$(INTDIR)\winproc.obj"

"$(OUTDIR)\dmboids.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "dmboids - Win32 Debug"

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

ALL : "$(OUTDIR)\dmboids.exe"

CLEAN : 
	-@erase "$(INTDIR)\boids.obj"
	-@erase "$(INTDIR)\d3dtex.obj"
	-@erase "$(INTDIR)\d3dutils.obj"
	-@erase "$(INTDIR)\d3dwin.obj"
	-@erase "$(INTDIR)\debug.obj"
	-@erase "$(INTDIR)\dmboids.res"
	-@erase "$(INTDIR)\drvmgr.obj"
	-@erase "$(INTDIR)\flock.obj"
	-@erase "$(INTDIR)\gull.obj"
	-@erase "$(INTDIR)\input.obj"
	-@erase "$(INTDIR)\music.obj"
	-@erase "$(INTDIR)\vc40.idb"
	-@erase "$(INTDIR)\vc40.pdb"
	-@erase "$(INTDIR)\winmain.obj"
	-@erase "$(INTDIR)\winproc.obj"
	-@erase "$(OUTDIR)\dmboids.exe"
	-@erase "$(OUTDIR)\dmboids.ilk"
	-@erase "$(OUTDIR)\dmboids.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /c
CPP_PROJ=/nologo /MLd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS"\
 /Fp"$(INTDIR)/dmboids.pch" /YX /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c 
CPP_OBJS=.\Debug/
CPP_SBRS=.\.
# ADD BASE MTL /nologo /D "_DEBUG" /win32
# ADD MTL /nologo /D "_DEBUG" /win32
MTL_PROJ=/nologo /D "_DEBUG" /win32 
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
RSC_PROJ=/l 0x409 /fo"$(INTDIR)/dmboids.res" /d "_DEBUG" 
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/dmboids.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib /nologo /subsystem:windows /debug /machine:I386
# ADD LINK32 ddraw.lib dsound.lib dinput.lib dxguid.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib /nologo /subsystem:windows /debug /machine:I386
LINK32_FLAGS=ddraw.lib dsound.lib dinput.lib dxguid.lib kernel32.lib user32.lib\
 gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib\
 oleaut32.lib uuid.lib /nologo /subsystem:windows /incremental:yes\
 /pdb:"$(OUTDIR)/dmboids.pdb" /debug /machine:I386 /out:"$(OUTDIR)/dmboids.exe" 
LINK32_OBJS= \
	"$(INTDIR)\boids.obj" \
	"$(INTDIR)\d3dtex.obj" \
	"$(INTDIR)\d3dutils.obj" \
	"$(INTDIR)\d3dwin.obj" \
	"$(INTDIR)\debug.obj" \
	"$(INTDIR)\dmboids.res" \
	"$(INTDIR)\drvmgr.obj" \
	"$(INTDIR)\flock.obj" \
	"$(INTDIR)\gull.obj" \
	"$(INTDIR)\input.obj" \
	"$(INTDIR)\music.obj" \
	"$(INTDIR)\winmain.obj" \
	"$(INTDIR)\winproc.obj"

"$(OUTDIR)\dmboids.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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

# Name "dmboids - Win32 Release"
# Name "dmboids - Win32 Debug"

!IF  "$(CFG)" == "dmboids - Win32 Release"

!ELSEIF  "$(CFG)" == "dmboids - Win32 Debug"

!ENDIF 

################################################################################
# Begin Source File

SOURCE=.\boids.cpp
DEP_CPP_BOIDS=\
	"..\..\..\..\..\INCLUDE\d3dcaps.h"\
	"..\..\..\..\..\INCLUDE\d3dvec.inl"\
	"..\..\..\..\..\INCLUDE\dls1.h"\
	"..\..\..\..\..\INCLUDE\dmdls.h"\
	"..\..\..\..\..\INCLUDE\dmerror.h"\
	".\boids.h"\
	".\common.h"\
	".\d3dscene.h"\
	".\d3dtex.h"\
	".\d3dutils.h"\
	".\d3dutils.inl"\
	".\d3dwin.h"\
	".\debug.h"\
	".\drvmgr.h"\
	".\input.h"\
	".\music.h"\
	{$(INCLUDE)}"\d3d.h"\
	{$(INCLUDE)}"\d3dtypes.h"\
	{$(INCLUDE)}"\dmusicc.h"\
	{$(INCLUDE)}"\dmusici.h"\
	

"$(INTDIR)\boids.obj" : $(SOURCE) $(DEP_CPP_BOIDS) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\boids.h

!IF  "$(CFG)" == "dmboids - Win32 Release"

!ELSEIF  "$(CFG)" == "dmboids - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\common.h

!IF  "$(CFG)" == "dmboids - Win32 Release"

!ELSEIF  "$(CFG)" == "dmboids - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\d3dscene.h

!IF  "$(CFG)" == "dmboids - Win32 Release"

!ELSEIF  "$(CFG)" == "dmboids - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\d3dtex.cpp
DEP_CPP_D3DTE=\
	"..\..\..\..\..\INCLUDE\d3dcaps.h"\
	"..\..\..\..\..\INCLUDE\d3dvec.inl"\
	".\d3dtex.h"\
	{$(INCLUDE)}"\d3d.h"\
	{$(INCLUDE)}"\d3dtypes.h"\
	

"$(INTDIR)\d3dtex.obj" : $(SOURCE) $(DEP_CPP_D3DTE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\d3dtex.h

!IF  "$(CFG)" == "dmboids - Win32 Release"

!ELSEIF  "$(CFG)" == "dmboids - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\d3dutils.cpp
DEP_CPP_D3DUT=\
	"..\..\..\..\..\INCLUDE\d3dcaps.h"\
	"..\..\..\..\..\INCLUDE\d3dvec.inl"\
	".\d3dutils.h"\
	".\d3dutils.inl"\
	{$(INCLUDE)}"\d3d.h"\
	{$(INCLUDE)}"\d3dtypes.h"\
	

"$(INTDIR)\d3dutils.obj" : $(SOURCE) $(DEP_CPP_D3DUT) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\d3dutils.h

!IF  "$(CFG)" == "dmboids - Win32 Release"

!ELSEIF  "$(CFG)" == "dmboids - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\d3dwin.cpp
DEP_CPP_D3DWI=\
	"..\..\..\..\..\INCLUDE\d3dcaps.h"\
	"..\..\..\..\..\INCLUDE\d3dvec.inl"\
	".\common.h"\
	".\d3dscene.h"\
	".\d3dwin.h"\
	".\debug.h"\
	".\drvmgr.h"\
	".\winproc.h"\
	{$(INCLUDE)}"\d3d.h"\
	{$(INCLUDE)}"\d3dtypes.h"\
	

"$(INTDIR)\d3dwin.obj" : $(SOURCE) $(DEP_CPP_D3DWI) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\d3dwin.h

!IF  "$(CFG)" == "dmboids - Win32 Release"

!ELSEIF  "$(CFG)" == "dmboids - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\debug.cpp
DEP_CPP_DEBUG=\
	"..\..\..\..\..\INCLUDE\d3dcaps.h"\
	"..\..\..\..\..\INCLUDE\d3dvec.inl"\
	".\common.h"\
	".\d3dwin.h"\
	".\debug.h"\
	".\drvmgr.h"\
	".\winmain.h"\
	".\winproc.h"\
	{$(INCLUDE)}"\d3d.h"\
	{$(INCLUDE)}"\d3dtypes.h"\
	

"$(INTDIR)\debug.obj" : $(SOURCE) $(DEP_CPP_DEBUG) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\debug.h

!IF  "$(CFG)" == "dmboids - Win32 Release"

!ELSEIF  "$(CFG)" == "dmboids - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\dmboids.rc
DEP_RSC_DMBOI=\
	".\d3d.ico"\
	".\dx5.bmp"\
	

"$(INTDIR)\dmboids.res" : $(SOURCE) $(DEP_RSC_DMBOI) "$(INTDIR)"
   $(RSC) $(RSC_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=.\drvmgr.cpp
DEP_CPP_DRVMG=\
	"..\..\..\..\..\INCLUDE\d3dcaps.h"\
	"..\..\..\..\..\INCLUDE\d3dvec.inl"\
	".\common.h"\
	".\debug.h"\
	".\drvmgr.h"\
	{$(INCLUDE)}"\d3d.h"\
	{$(INCLUDE)}"\d3dtypes.h"\
	

"$(INTDIR)\drvmgr.obj" : $(SOURCE) $(DEP_CPP_DRVMG) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\drvmgr.h

!IF  "$(CFG)" == "dmboids - Win32 Release"

!ELSEIF  "$(CFG)" == "dmboids - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\flock.cpp
DEP_CPP_FLOCK=\
	"..\..\..\..\..\INCLUDE\d3dcaps.h"\
	"..\..\..\..\..\INCLUDE\d3dvec.inl"\
	"..\..\..\..\..\INCLUDE\dls1.h"\
	"..\..\..\..\..\INCLUDE\dmdls.h"\
	"..\..\..\..\..\INCLUDE\dmerror.h"\
	".\boids.h"\
	".\common.h"\
	".\d3dutils.h"\
	".\d3dutils.inl"\
	".\debug.h"\
	".\input.h"\
	".\music.h"\
	{$(INCLUDE)}"\d3d.h"\
	{$(INCLUDE)}"\d3dtypes.h"\
	{$(INCLUDE)}"\dmusicc.h"\
	{$(INCLUDE)}"\dmusici.h"\
	

"$(INTDIR)\flock.obj" : $(SOURCE) $(DEP_CPP_FLOCK) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\gull.cpp
DEP_CPP_GULL_=\
	"..\..\..\..\..\INCLUDE\d3dcaps.h"\
	"..\..\..\..\..\INCLUDE\d3dvec.inl"\
	".\boids.h"\
	".\common.h"\
	".\d3dscene.h"\
	".\d3dtex.h"\
	".\d3dutils.h"\
	".\d3dutils.inl"\
	".\d3dwin.h"\
	".\debug.h"\
	".\drvmgr.h"\
	{$(INCLUDE)}"\d3d.h"\
	{$(INCLUDE)}"\d3dtypes.h"\
	

"$(INTDIR)\gull.obj" : $(SOURCE) $(DEP_CPP_GULL_) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\input.cpp
DEP_CPP_INPUT=\
	"..\..\..\..\..\INCLUDE\d3dcaps.h"\
	"..\..\..\..\..\INCLUDE\d3dvec.inl"\
	"..\..\..\..\..\INCLUDE\dls1.h"\
	"..\..\..\..\..\INCLUDE\dmdls.h"\
	"..\..\..\..\..\INCLUDE\dmerror.h"\
	".\common.h"\
	".\d3dscene.h"\
	".\d3dwin.h"\
	".\debug.h"\
	".\drvmgr.h"\
	".\input.h"\
	".\music.h"\
	".\winmain.h"\
	".\winproc.h"\
	{$(INCLUDE)}"\d3d.h"\
	{$(INCLUDE)}"\d3dtypes.h"\
	{$(INCLUDE)}"\dinput.h"\
	{$(INCLUDE)}"\dmusicc.h"\
	{$(INCLUDE)}"\dmusici.h"\
	

"$(INTDIR)\input.obj" : $(SOURCE) $(DEP_CPP_INPUT) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\input.h

!IF  "$(CFG)" == "dmboids - Win32 Release"

!ELSEIF  "$(CFG)" == "dmboids - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\music.cpp
DEP_CPP_MUSIC=\
	"..\..\..\..\..\INCLUDE\d3dcaps.h"\
	"..\..\..\..\..\INCLUDE\d3dvec.inl"\
	"..\..\..\..\..\INCLUDE\dls1.h"\
	"..\..\..\..\..\INCLUDE\dmdls.h"\
	"..\..\..\..\..\INCLUDE\dmerror.h"\
	".\common.h"\
	".\music.h"\
	{$(INCLUDE)}"\d3d.h"\
	{$(INCLUDE)}"\d3dtypes.h"\
	{$(INCLUDE)}"\dmusicc.h"\
	{$(INCLUDE)}"\dmusici.h"\
	

"$(INTDIR)\music.obj" : $(SOURCE) $(DEP_CPP_MUSIC) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\music.h

!IF  "$(CFG)" == "dmboids - Win32 Release"

!ELSEIF  "$(CFG)" == "dmboids - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\resource.h

!IF  "$(CFG)" == "dmboids - Win32 Release"

!ELSEIF  "$(CFG)" == "dmboids - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\winmain.cpp
DEP_CPP_WINMA=\
	"..\..\..\..\..\INCLUDE\d3dcaps.h"\
	"..\..\..\..\..\INCLUDE\d3dvec.inl"\
	"..\..\..\..\..\INCLUDE\dls1.h"\
	"..\..\..\..\..\INCLUDE\dmdls.h"\
	"..\..\..\..\..\INCLUDE\dmerror.h"\
	".\boids.h"\
	".\common.h"\
	".\d3dscene.h"\
	".\d3dwin.h"\
	".\debug.h"\
	".\drvmgr.h"\
	".\input.h"\
	".\music.h"\
	".\winmain.h"\
	".\winproc.h"\
	{$(INCLUDE)}"\d3d.h"\
	{$(INCLUDE)}"\d3dtypes.h"\
	{$(INCLUDE)}"\dmusicc.h"\
	{$(INCLUDE)}"\dmusici.h"\
	

"$(INTDIR)\winmain.obj" : $(SOURCE) $(DEP_CPP_WINMA) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\winmain.h

!IF  "$(CFG)" == "dmboids - Win32 Release"

!ELSEIF  "$(CFG)" == "dmboids - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\winproc.cpp
DEP_CPP_WINPR=\
	"..\..\..\..\..\INCLUDE\d3dcaps.h"\
	"..\..\..\..\..\INCLUDE\d3dvec.inl"\
	".\common.h"\
	".\d3dwin.h"\
	".\debug.h"\
	".\drvmgr.h"\
	".\winmain.h"\
	".\winproc.h"\
	{$(INCLUDE)}"\d3d.h"\
	{$(INCLUDE)}"\d3dtypes.h"\
	

"$(INTDIR)\winproc.obj" : $(SOURCE) $(DEP_CPP_WINPR) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\winproc.h

!IF  "$(CFG)" == "dmboids - Win32 Release"

!ELSEIF  "$(CFG)" == "dmboids - Win32 Debug"

!ENDIF 

# End Source File
# End Target
# End Project
################################################################################

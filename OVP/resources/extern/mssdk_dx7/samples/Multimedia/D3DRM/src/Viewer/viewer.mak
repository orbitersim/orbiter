# Microsoft Developer Studio Generated NMAKE File, Format Version 4.20
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Application" 0x0101

!IF "$(CFG)" == ""
CFG=viewer - Win32 Debug
!MESSAGE No configuration specified.  Defaulting to viewer - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "viewer - Win32 Release" && "$(CFG)" != "viewer - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "viewer.mak" CFG="viewer - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "viewer - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "viewer - Win32 Debug" (based on "Win32 (x86) Application")
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
# PROP Target_Last_Scanned "viewer - Win32 Debug"
RSC=rc.exe
MTL=mktyplib.exe
CPP=cl.exe

!IF  "$(CFG)" == "viewer - Win32 Release"

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

ALL : "$(OUTDIR)\viewer.exe"

CLEAN : 
	-@erase "$(INTDIR)\color.obj"
	-@erase "$(INTDIR)\file.obj"
	-@erase "$(INTDIR)\rodcone.obj"
	-@erase "$(INTDIR)\sel.obj"
	-@erase "$(INTDIR)\viewer.obj"
	-@erase "$(INTDIR)\viewer.res"
	-@erase "$(OUTDIR)\viewer.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /c
CPP_PROJ=/nologo /ML /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS"\
 /Fp"$(INTDIR)/viewer.pch" /YX /Fo"$(INTDIR)/" /c 
CPP_OBJS=.\Release/
CPP_SBRS=.\.
# ADD BASE MTL /nologo /D "NDEBUG" /win32
# ADD MTL /nologo /D "NDEBUG" /win32
MTL_PROJ=/nologo /D "NDEBUG" /win32 
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
RSC_PROJ=/l 0x409 /fo"$(INTDIR)/viewer.res" /d "NDEBUG" 
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/viewer.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib /nologo /subsystem:windows /machine:I386
# ADD LINK32 dxguid.lib ddraw.lib d3drm.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib /nologo /subsystem:windows /machine:I386
LINK32_FLAGS=dxguid.lib ddraw.lib d3drm.lib kernel32.lib user32.lib gdi32.lib\
 winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib\
 /nologo /subsystem:windows /incremental:no /pdb:"$(OUTDIR)/viewer.pdb"\
 /machine:I386 /out:"$(OUTDIR)/viewer.exe" 
LINK32_OBJS= \
	"$(INTDIR)\color.obj" \
	"$(INTDIR)\file.obj" \
	"$(INTDIR)\rodcone.obj" \
	"$(INTDIR)\sel.obj" \
	"$(INTDIR)\viewer.obj" \
	"$(INTDIR)\viewer.res"

"$(OUTDIR)\viewer.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "viewer - Win32 Debug"

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

ALL : "$(OUTDIR)\viewer.exe"

CLEAN : 
	-@erase "$(INTDIR)\color.obj"
	-@erase "$(INTDIR)\file.obj"
	-@erase "$(INTDIR)\rodcone.obj"
	-@erase "$(INTDIR)\sel.obj"
	-@erase "$(INTDIR)\vc40.idb"
	-@erase "$(INTDIR)\vc40.pdb"
	-@erase "$(INTDIR)\viewer.obj"
	-@erase "$(INTDIR)\viewer.res"
	-@erase "$(OUTDIR)\viewer.exe"
	-@erase "$(OUTDIR)\viewer.ilk"
	-@erase "$(OUTDIR)\viewer.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /c
CPP_PROJ=/nologo /MLd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS"\
 /Fp"$(INTDIR)/viewer.pch" /YX /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c 
CPP_OBJS=.\Debug/
CPP_SBRS=.\.
# ADD BASE MTL /nologo /D "_DEBUG" /win32
# ADD MTL /nologo /D "_DEBUG" /win32
MTL_PROJ=/nologo /D "_DEBUG" /win32 
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
RSC_PROJ=/l 0x409 /fo"$(INTDIR)/viewer.res" /d "_DEBUG" 
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/viewer.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib /nologo /subsystem:windows /debug /machine:I386
# ADD LINK32 dxguid.lib ddraw.lib d3drm.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib /nologo /subsystem:windows /debug /machine:I386
LINK32_FLAGS=dxguid.lib ddraw.lib d3drm.lib kernel32.lib user32.lib gdi32.lib\
 winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib\
 /nologo /subsystem:windows /incremental:yes /pdb:"$(OUTDIR)/viewer.pdb" /debug\
 /machine:I386 /out:"$(OUTDIR)/viewer.exe" 
LINK32_OBJS= \
	"$(INTDIR)\color.obj" \
	"$(INTDIR)\file.obj" \
	"$(INTDIR)\rodcone.obj" \
	"$(INTDIR)\sel.obj" \
	"$(INTDIR)\viewer.obj" \
	"$(INTDIR)\viewer.res"

"$(OUTDIR)\viewer.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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

# Name "viewer - Win32 Release"
# Name "viewer - Win32 Debug"

!IF  "$(CFG)" == "viewer - Win32 Release"

!ELSEIF  "$(CFG)" == "viewer - Win32 Debug"

!ENDIF 

################################################################################
# Begin Source File

SOURCE=.\sel.cpp
DEP_CPP_SEL_C=\
	"..\..\..\..\..\INCLUDE\d3d.h"\
	"..\..\..\..\..\INCLUDE\d3dcaps.h"\
	"..\..\..\..\..\INCLUDE\d3drmdef.h"\
	"..\..\..\..\..\INCLUDE\d3drmobj.h"\
	"..\..\..\..\..\INCLUDE\d3dtypes.h"\
	"..\..\..\..\..\INCLUDE\d3dvec.inl"\
	".\rodcone.h"\
	".\sel.h"\
	".\viewer.h"\
	{$(INCLUDE)}"\d3drm.h"\
	{$(INCLUDE)}"\d3drmwin.h"\
	

"$(INTDIR)\sel.obj" : $(SOURCE) $(DEP_CPP_SEL_C) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\file.cpp
DEP_CPP_FILE_=\
	"..\..\..\..\..\INCLUDE\d3d.h"\
	"..\..\..\..\..\INCLUDE\d3dcaps.h"\
	"..\..\..\..\..\INCLUDE\d3drmdef.h"\
	"..\..\..\..\..\INCLUDE\d3drmobj.h"\
	"..\..\..\..\..\INCLUDE\d3dtypes.h"\
	"..\..\..\..\..\INCLUDE\d3dvec.inl"\
	".\viewer.h"\
	{$(INCLUDE)}"\d3drm.h"\
	{$(INCLUDE)}"\d3drmwin.h"\
	

"$(INTDIR)\file.obj" : $(SOURCE) $(DEP_CPP_FILE_) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\rodcone.cpp
DEP_CPP_RODCO=\
	"..\..\..\..\..\INCLUDE\d3d.h"\
	"..\..\..\..\..\INCLUDE\d3dcaps.h"\
	"..\..\..\..\..\INCLUDE\d3drmdef.h"\
	"..\..\..\..\..\INCLUDE\d3drmobj.h"\
	"..\..\..\..\..\INCLUDE\d3dtypes.h"\
	"..\..\..\..\..\INCLUDE\d3dvec.inl"\
	".\rodcone.h"\
	".\viewer.h"\
	{$(INCLUDE)}"\d3drm.h"\
	{$(INCLUDE)}"\d3drmwin.h"\
	

"$(INTDIR)\rodcone.obj" : $(SOURCE) $(DEP_CPP_RODCO) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\rodcone.h

!IF  "$(CFG)" == "viewer - Win32 Release"

!ELSEIF  "$(CFG)" == "viewer - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\color.cpp
DEP_CPP_COLOR=\
	"..\..\..\..\..\INCLUDE\d3d.h"\
	"..\..\..\..\..\INCLUDE\d3dcaps.h"\
	"..\..\..\..\..\INCLUDE\d3drmdef.h"\
	"..\..\..\..\..\INCLUDE\d3drmobj.h"\
	"..\..\..\..\..\INCLUDE\d3dtypes.h"\
	"..\..\..\..\..\INCLUDE\d3dvec.inl"\
	".\viewer.h"\
	{$(INCLUDE)}"\d3drm.h"\
	{$(INCLUDE)}"\d3drmwin.h"\
	

"$(INTDIR)\color.obj" : $(SOURCE) $(DEP_CPP_COLOR) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\sel.h

!IF  "$(CFG)" == "viewer - Win32 Release"

!ELSEIF  "$(CFG)" == "viewer - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\viewer.cpp
DEP_CPP_VIEWE=\
	"..\..\..\..\..\INCLUDE\d3d.h"\
	"..\..\..\..\..\INCLUDE\d3dcaps.h"\
	"..\..\..\..\..\INCLUDE\d3drmdef.h"\
	"..\..\..\..\..\INCLUDE\d3drmobj.h"\
	"..\..\..\..\..\INCLUDE\d3dtypes.h"\
	"..\..\..\..\..\INCLUDE\d3dvec.inl"\
	".\sel.h"\
	".\viewer.h"\
	{$(INCLUDE)}"\d3drm.h"\
	{$(INCLUDE)}"\d3drmwin.h"\
	

"$(INTDIR)\viewer.obj" : $(SOURCE) $(DEP_CPP_VIEWE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\viewer.h

!IF  "$(CFG)" == "viewer - Win32 Release"

!ELSEIF  "$(CFG)" == "viewer - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\viewer.rc
DEP_RSC_VIEWER=\
	".\viewer.h"\
	".\viewer.ico"\
	

"$(INTDIR)\viewer.res" : $(SOURCE) $(DEP_RSC_VIEWER) "$(INTDIR)"
   $(RSC) $(RSC_PROJ) $(SOURCE)


# End Source File
# End Target
# End Project
################################################################################

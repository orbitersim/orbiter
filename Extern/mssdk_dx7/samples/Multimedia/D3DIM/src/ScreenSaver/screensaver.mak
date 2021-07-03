# Microsoft Developer Studio Generated NMAKE File, Format Version 4.20
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Application" 0x0101

!IF "$(CFG)" == ""
CFG=Screensaver - Win32 Debug
!MESSAGE No configuration specified.  Defaulting to Screensaver - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "Screensaver - Win32 Release" && "$(CFG)" !=\
 "Screensaver - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "screensaver.mak" CFG="Screensaver - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "Screensaver - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "Screensaver - Win32 Debug" (based on "Win32 (x86) Application")
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
# PROP Target_Last_Scanned "Screensaver - Win32 Debug"
CPP=cl.exe
MTL=mktyplib.exe
RSC=rc.exe

!IF  "$(CFG)" == "Screensaver - Win32 Release"

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

ALL : "$(OUTDIR)\D3D.scr"

CLEAN : 
	-@erase "$(INTDIR)\render.obj"
	-@erase "$(INTDIR)\screensaver.obj"
	-@erase "$(INTDIR)\screensaver.res"
	-@erase "$(OUTDIR)\D3D.scr"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /W3 /GX /O2 /I "..\..\include" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /c
# SUBTRACT CPP /YX
CPP_PROJ=/nologo /ML /W3 /GX /O2 /I "..\..\include" /D "WIN32" /D "NDEBUG" /D\
 "_WINDOWS" /Fo"$(INTDIR)/" /c 
CPP_OBJS=.\Release/
CPP_SBRS=.\.
# ADD BASE MTL /nologo /D "NDEBUG" /win32
# ADD MTL /nologo /D "NDEBUG" /win32
MTL_PROJ=/nologo /D "NDEBUG" /win32 
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
RSC_PROJ=/l 0x409 /fo"$(INTDIR)/screensaver.res" /d "NDEBUG" 
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/screensaver.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /machine:I386
# ADD LINK32 scrnsave.lib ddraw.lib winmm.lib dxguid.lib ..\..\lib\d3dframe.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib /nologo /subsystem:windows /machine:I386 /out:"Release/D3D.scr"
LINK32_FLAGS=scrnsave.lib ddraw.lib winmm.lib dxguid.lib ..\..\lib\d3dframe.lib\
 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib\
 shell32.lib ole32.lib oleaut32.lib uuid.lib /nologo /subsystem:windows\
 /incremental:no /pdb:"$(OUTDIR)/D3D.pdb" /machine:I386 /out:"$(OUTDIR)/D3D.scr"\
 
LINK32_OBJS= \
	"$(INTDIR)\render.obj" \
	"$(INTDIR)\screensaver.obj" \
	"$(INTDIR)\screensaver.res"

"$(OUTDIR)\D3D.scr" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "Screensaver - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Screensa"
# PROP BASE Intermediate_Dir "Screensa"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
OUTDIR=.\Debug
INTDIR=.\Debug

ALL : "$(OUTDIR)\D3D.scr"

CLEAN : 
	-@erase "$(INTDIR)\render.obj"
	-@erase "$(INTDIR)\screensaver.obj"
	-@erase "$(INTDIR)\screensaver.res"
	-@erase "$(INTDIR)\vc40.idb"
	-@erase "$(INTDIR)\vc40.pdb"
	-@erase "$(OUTDIR)\D3D.ilk"
	-@erase "$(OUTDIR)\D3D.pdb"
	-@erase "$(OUTDIR)\D3D.scr"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /W3 /Gm /GX /Zi /Od /I "..\..\include" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /c
# SUBTRACT CPP /YX
CPP_PROJ=/nologo /MLd /W3 /Gm /GX /Zi /Od /I "..\..\include" /D "WIN32" /D\
 "_DEBUG" /D "_WINDOWS" /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c 
CPP_OBJS=.\Debug/
CPP_SBRS=.\.
# ADD BASE MTL /nologo /D "_DEBUG" /win32
# ADD MTL /nologo /D "_DEBUG" /win32
MTL_PROJ=/nologo /D "_DEBUG" /win32 
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
RSC_PROJ=/l 0x409 /fo"$(INTDIR)/screensaver.res" /d "_DEBUG" 
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/screensaver.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /debug /machine:I386
# ADD LINK32 scrnsave.lib ddraw.lib winmm.lib dxguid.lib ..\..\lib\d3dframe.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib /nologo /subsystem:windows /debug /machine:I386 /out:"Debug\D3D.scr"
LINK32_FLAGS=scrnsave.lib ddraw.lib winmm.lib dxguid.lib ..\..\lib\d3dframe.lib\
 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib\
 shell32.lib ole32.lib oleaut32.lib uuid.lib /nologo /subsystem:windows\
 /incremental:yes /pdb:"$(OUTDIR)/D3D.pdb" /debug /machine:I386\
 /out:"$(OUTDIR)/D3D.scr" 
LINK32_OBJS= \
	"$(INTDIR)\render.obj" \
	"$(INTDIR)\screensaver.obj" \
	"$(INTDIR)\screensaver.res"

"$(OUTDIR)\D3D.scr" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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

# Name "Screensaver - Win32 Release"
# Name "Screensaver - Win32 Debug"

!IF  "$(CFG)" == "Screensaver - Win32 Release"

!ELSEIF  "$(CFG)" == "Screensaver - Win32 Debug"

!ENDIF 

################################################################################
# Begin Source File

SOURCE=.\screensaver.rc
DEP_RSC_SCREE=\
	".\DirectX.ico"\
	

"$(INTDIR)\screensaver.res" : $(SOURCE) $(DEP_RSC_SCREE) "$(INTDIR)"
   $(RSC) $(RSC_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=.\resource.h

!IF  "$(CFG)" == "Screensaver - Win32 Release"

!ELSEIF  "$(CFG)" == "Screensaver - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\screensaver.cpp
DEP_CPP_SCREEN=\
	"..\..\include\D3DEnum.h"\
	"..\..\include\D3DFrame.h"\
	"..\..\include\D3DUtil.h"\
	".\screensaver.h"\
	"C:\MSSDK\INCLUDE\d3dcaps.h"\
	"C:\MSSDK\INCLUDE\d3dtypes.h"\
	"C:\MSSDK\INCLUDE\d3dvec.inl"\
	{$(INCLUDE)}"\d3d.h"\
	

"$(INTDIR)\screensaver.obj" : $(SOURCE) $(DEP_CPP_SCREEN) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\screensaver.h

!IF  "$(CFG)" == "Screensaver - Win32 Release"

!ELSEIF  "$(CFG)" == "Screensaver - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\render.cpp
DEP_CPP_RENDE=\
	"..\..\include\D3DTextr.h"\
	"..\..\include\D3DUtil.h"\
	"C:\MSSDK\INCLUDE\d3dcaps.h"\
	"C:\MSSDK\INCLUDE\d3dtypes.h"\
	"C:\MSSDK\INCLUDE\d3dvec.inl"\
	{$(INCLUDE)}"\d3d.h"\
	

"$(INTDIR)\render.obj" : $(SOURCE) $(DEP_CPP_RENDE) "$(INTDIR)"


# End Source File
# End Target
# End Project
################################################################################

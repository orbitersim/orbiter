# Microsoft Developer Studio Generated NMAKE File, Format Version 4.20
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Application" 0x0101

!IF "$(CFG)" == ""
CFG=shadowvol2 - Win32 Debug
!MESSAGE No configuration specified.  Defaulting to shadowvol2 - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "shadowvol2 - Win32 Release" && "$(CFG)" !=\
 "shadowvol2 - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "shadowvol2.mak" CFG="shadowvol2 - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "shadowvol2 - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "shadowvol2 - Win32 Debug" (based on "Win32 (x86) Application")
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
# PROP Target_Last_Scanned "shadowvol2 - Win32 Debug"
MTL=mktyplib.exe
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "shadowvol2 - Win32 Release"

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

ALL : "$(OUTDIR)\shadowvol2.exe"

CLEAN : 
	-@erase "$(INTDIR)\chull.obj"
	-@erase "$(INTDIR)\geom.obj"
	-@erase "$(INTDIR)\shadowvol2.obj"
	-@erase "$(INTDIR)\winmain.res"
	-@erase "$(OUTDIR)\shadowvol2.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /W3 /GX /O2 /I "..\..\include" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /c
CPP_PROJ=/nologo /ML /W3 /GX /O2 /I "..\..\include" /D "WIN32" /D "NDEBUG" /D\
 "_WINDOWS" /Fp"$(INTDIR)/shadowvol2.pch" /YX /Fo"$(INTDIR)/" /c 
CPP_OBJS=.\Release/
CPP_SBRS=.\.
# ADD BASE MTL /nologo /D "NDEBUG" /win32
# ADD MTL /nologo /D "NDEBUG" /win32
MTL_PROJ=/nologo /D "NDEBUG" /win32 
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
RSC_PROJ=/l 0x409 /fo"$(INTDIR)/winmain.res" /d "NDEBUG" 
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/shadowvol2.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /machine:I386
# ADD LINK32 dxguid.lib winmm.lib ddraw.lib ..\..\lib\d3dframe.lib user32.lib gdi32.lib kernel32.lib advapi32.lib /nologo /subsystem:windows /machine:I386
LINK32_FLAGS=dxguid.lib winmm.lib ddraw.lib ..\..\lib\d3dframe.lib user32.lib\
 gdi32.lib kernel32.lib advapi32.lib /nologo /subsystem:windows /incremental:no\
 /pdb:"$(OUTDIR)/shadowvol2.pdb" /machine:I386 /out:"$(OUTDIR)/shadowvol2.exe" 
LINK32_OBJS= \
	"$(INTDIR)\chull.obj" \
	"$(INTDIR)\geom.obj" \
	"$(INTDIR)\shadowvol2.obj" \
	"$(INTDIR)\winmain.res"

"$(OUTDIR)\shadowvol2.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "shadowvol2 - Win32 Debug"

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

ALL : "$(OUTDIR)\shadowvol2.exe"

CLEAN : 
	-@erase "$(INTDIR)\chull.obj"
	-@erase "$(INTDIR)\geom.obj"
	-@erase "$(INTDIR)\shadowvol2.obj"
	-@erase "$(INTDIR)\vc40.idb"
	-@erase "$(INTDIR)\vc40.pdb"
	-@erase "$(INTDIR)\winmain.res"
	-@erase "$(OUTDIR)\shadowvol2.exe"
	-@erase "$(OUTDIR)\shadowvol2.ilk"
	-@erase "$(OUTDIR)\shadowvol2.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /W3 /Gm /GX /Zi /Od /I "..\..\include" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /c
CPP_PROJ=/nologo /MLd /W3 /Gm /GX /Zi /Od /I "..\..\include" /D "WIN32" /D\
 "_DEBUG" /D "_WINDOWS" /Fp"$(INTDIR)/shadowvol2.pch" /YX /Fo"$(INTDIR)/"\
 /Fd"$(INTDIR)/" /c 
CPP_OBJS=.\Debug/
CPP_SBRS=.\.
# ADD BASE MTL /nologo /D "_DEBUG" /win32
# ADD MTL /nologo /D "_DEBUG" /win32
MTL_PROJ=/nologo /D "_DEBUG" /win32 
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
RSC_PROJ=/l 0x409 /fo"$(INTDIR)/winmain.res" /d "_DEBUG" 
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/shadowvol2.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /debug /machine:I386
# ADD LINK32 dxguid.lib winmm.lib ddraw.lib ..\..\lib\d3dframe.lib user32.lib gdi32.lib kernel32.lib advapi32.lib /nologo /subsystem:windows /debug /machine:I386
LINK32_FLAGS=dxguid.lib winmm.lib ddraw.lib ..\..\lib\d3dframe.lib user32.lib\
 gdi32.lib kernel32.lib advapi32.lib /nologo /subsystem:windows /incremental:yes\
 /pdb:"$(OUTDIR)/shadowvol2.pdb" /debug /machine:I386\
 /out:"$(OUTDIR)/shadowvol2.exe" 
LINK32_OBJS= \
	"$(INTDIR)\chull.obj" \
	"$(INTDIR)\geom.obj" \
	"$(INTDIR)\shadowvol2.obj" \
	"$(INTDIR)\winmain.res"

"$(OUTDIR)\shadowvol2.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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

# Name "shadowvol2 - Win32 Release"
# Name "shadowvol2 - Win32 Debug"

!IF  "$(CFG)" == "shadowvol2 - Win32 Release"

!ELSEIF  "$(CFG)" == "shadowvol2 - Win32 Debug"

!ENDIF 

################################################################################
# Begin Source File

SOURCE=.\chull.cpp
DEP_CPP_CHULL=\
	"..\..\include\D3DEnum.h"\
	"..\..\include\D3DMath.h"\
	"..\..\include\D3DTextr.h"\
	"..\..\include\D3DUtil.h"\
	".\shadow.h"\
	"c:\mssdk\include\d3dcaps.h"\
	"c:\mssdk\include\d3dtypes.h"\
	"c:\mssdk\include\d3dvec.inl"\
	{$(INCLUDE)}"\d3d.h"\
	

"$(INTDIR)\chull.obj" : $(SOURCE) $(DEP_CPP_CHULL) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\resource.h

!IF  "$(CFG)" == "shadowvol2 - Win32 Release"

!ELSEIF  "$(CFG)" == "shadowvol2 - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\shadowvol2.cpp
DEP_CPP_SHADO=\
	"..\..\include\D3DApp.h"\
	"..\..\include\D3DEnum.h"\
	"..\..\include\D3DFrame.h"\
	"..\..\include\D3DMath.h"\
	"..\..\include\D3DRes.h"\
	"..\..\include\D3DTextr.h"\
	"..\..\include\D3DUtil.h"\
	".\shadow.h"\
	"c:\mssdk\include\d3dcaps.h"\
	"c:\mssdk\include\d3dtypes.h"\
	"c:\mssdk\include\d3dvec.inl"\
	{$(INCLUDE)}"\d3d.h"\
	

"$(INTDIR)\shadowvol2.obj" : $(SOURCE) $(DEP_CPP_SHADO) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\winmain.rc
DEP_RSC_WINMA=\
	".\DirectX.ico"\
	

"$(INTDIR)\winmain.res" : $(SOURCE) $(DEP_RSC_WINMA) "$(INTDIR)"
   $(RSC) $(RSC_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=.\geom.cpp
DEP_CPP_GEOM_=\
	"..\..\include\D3DEnum.h"\
	"..\..\include\D3DMath.h"\
	"..\..\include\D3DTextr.h"\
	"..\..\include\D3DUtil.h"\
	".\shadow.h"\
	"c:\mssdk\include\d3dcaps.h"\
	"c:\mssdk\include\d3dtypes.h"\
	"c:\mssdk\include\d3dvec.inl"\
	{$(INCLUDE)}"\d3d.h"\
	

"$(INTDIR)\geom.obj" : $(SOURCE) $(DEP_CPP_GEOM_) "$(INTDIR)"


# End Source File
# End Target
# End Project
################################################################################

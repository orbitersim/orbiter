# Microsoft Developer Studio Generated NMAKE File, Format Version 4.20
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Application" 0x0101

!IF "$(CFG)" == ""
CFG=MFCFog - Win32 Debug
!MESSAGE No configuration specified.  Defaulting to MFCFog - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "MFCFog - Win32 Release" && "$(CFG)" != "MFCFog - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "MFCFog.mak" CFG="MFCFog - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "MFCFog - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "MFCFog - Win32 Debug" (based on "Win32 (x86) Application")
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
# PROP Target_Last_Scanned "MFCFog - Win32 Debug"
MTL=mktyplib.exe
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "MFCFog - Win32 Release"

# PROP BASE Use_MFC 6
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 6
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
OUTDIR=.\Release
INTDIR=.\Release

ALL : "$(OUTDIR)\MFCFog.exe"

CLEAN : 
	-@erase "$(INTDIR)\d3dapp.obj"
	-@erase "$(INTDIR)\d3dapp.res"
	-@erase "$(INTDIR)\fog.obj"
	-@erase "$(INTDIR)\stdafx.obj"
	-@erase "$(OUTDIR)\MFCFog.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /MD /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_AFXDLL" /D "_MBCS" /Yu"stdafx.h" /c
# ADD CPP /nologo /MD /W3 /GX /O2 /I "..\..\include" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_AFXDLL" /D "_MBCS" /YX /c
CPP_PROJ=/nologo /MD /W3 /GX /O2 /I "..\..\include" /D "WIN32" /D "NDEBUG" /D\
 "_WINDOWS" /D "_AFXDLL" /D "_MBCS" /Fp"$(INTDIR)/MFCFog.pch" /YX\
 /Fo"$(INTDIR)/" /c 
CPP_OBJS=.\Release/
CPP_SBRS=.\.
# ADD BASE MTL /nologo /D "NDEBUG" /win32
# ADD MTL /nologo /D "NDEBUG" /win32
MTL_PROJ=/nologo /D "NDEBUG" /win32 
# ADD BASE RSC /l 0x409 /d "NDEBUG" /d "_AFXDLL"
# ADD RSC /l 0x409 /d "NDEBUG" /d "_AFXDLL"
RSC_PROJ=/l 0x409 /fo"$(INTDIR)/d3dapp.res" /d "NDEBUG" /d "_AFXDLL" 
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/MFCFog.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
# ADD BASE LINK32 /nologo /subsystem:windows /machine:I386
# ADD LINK32 ddraw.lib winmm.lib dxguid.lib ..\..\lib\d3dframe.lib /nologo /subsystem:windows /machine:I386
LINK32_FLAGS=ddraw.lib winmm.lib dxguid.lib ..\..\lib\d3dframe.lib /nologo\
 /subsystem:windows /incremental:no /pdb:"$(OUTDIR)/MFCFog.pdb" /machine:I386\
 /out:"$(OUTDIR)/MFCFog.exe" 
LINK32_OBJS= \
	"$(INTDIR)\d3dapp.obj" \
	"$(INTDIR)\d3dapp.res" \
	"$(INTDIR)\fog.obj" \
	"$(INTDIR)\stdafx.obj"

"$(OUTDIR)\MFCFog.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "MFCFog - Win32 Debug"

# PROP BASE Use_MFC 6
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 6
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
OUTDIR=.\Debug
INTDIR=.\Debug

ALL : "$(OUTDIR)\MFCFog.exe"

CLEAN : 
	-@erase "$(INTDIR)\d3dapp.obj"
	-@erase "$(INTDIR)\d3dapp.res"
	-@erase "$(INTDIR)\fog.obj"
	-@erase "$(INTDIR)\stdafx.obj"
	-@erase "$(INTDIR)\vc40.idb"
	-@erase "$(INTDIR)\vc40.pdb"
	-@erase "$(OUTDIR)\MFCFog.exe"
	-@erase "$(OUTDIR)\MFCFog.ilk"
	-@erase "$(OUTDIR)\MFCFog.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /MDd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_AFXDLL" /D "_MBCS" /Yu"stdafx.h" /c
# ADD CPP /nologo /MDd /W3 /Gm /GX /Zi /Od /I "..\..\include" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_AFXDLL" /D "_MBCS" /YX /c
CPP_PROJ=/nologo /MDd /W3 /Gm /GX /Zi /Od /I "..\..\include" /D "WIN32" /D\
 "_DEBUG" /D "_WINDOWS" /D "_AFXDLL" /D "_MBCS" /Fp"$(INTDIR)/MFCFog.pch" /YX\
 /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c 
CPP_OBJS=.\Debug/
CPP_SBRS=.\.
# ADD BASE MTL /nologo /D "_DEBUG" /win32
# ADD MTL /nologo /D "_DEBUG" /win32
MTL_PROJ=/nologo /D "_DEBUG" /win32 
# ADD BASE RSC /l 0x409 /d "_DEBUG" /d "_AFXDLL"
# ADD RSC /l 0x409 /d "_DEBUG" /d "_AFXDLL"
RSC_PROJ=/l 0x409 /fo"$(INTDIR)/d3dapp.res" /d "_DEBUG" /d "_AFXDLL" 
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/MFCFog.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
# ADD BASE LINK32 /nologo /subsystem:windows /debug /machine:I386
# ADD LINK32 ddraw.lib winmm.lib dxguid.lib ..\..\lib\d3dframe.lib /nologo /subsystem:windows /debug /machine:I386
LINK32_FLAGS=ddraw.lib winmm.lib dxguid.lib ..\..\lib\d3dframe.lib /nologo\
 /subsystem:windows /incremental:yes /pdb:"$(OUTDIR)/MFCFog.pdb" /debug\
 /machine:I386 /out:"$(OUTDIR)/MFCFog.exe" 
LINK32_OBJS= \
	"$(INTDIR)\d3dapp.obj" \
	"$(INTDIR)\d3dapp.res" \
	"$(INTDIR)\fog.obj" \
	"$(INTDIR)\stdafx.obj"

"$(OUTDIR)\MFCFog.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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

# Name "MFCFog - Win32 Release"
# Name "MFCFog - Win32 Debug"

!IF  "$(CFG)" == "MFCFog - Win32 Release"

!ELSEIF  "$(CFG)" == "MFCFog - Win32 Debug"

!ENDIF 

################################################################################
# Begin Source File

SOURCE=.\stdafx.h

!IF  "$(CFG)" == "MFCFog - Win32 Release"

!ELSEIF  "$(CFG)" == "MFCFog - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\d3dapp.h

!IF  "$(CFG)" == "MFCFog - Win32 Release"

!ELSEIF  "$(CFG)" == "MFCFog - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\d3dapp.rc
DEP_RSC_D3DAP=\
	".\DirectX.ico"\
	

"$(INTDIR)\d3dapp.res" : $(SOURCE) $(DEP_RSC_D3DAP) "$(INTDIR)"
   $(RSC) $(RSC_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=.\fog.cpp
DEP_CPP_FOG_C=\
	"..\..\include\D3DMath.h"\
	"..\..\include\D3DTextr.h"\
	"..\..\include\D3DUtil.h"\
	"c:\mssdk\include\d3dcaps.h"\
	"c:\mssdk\include\d3dtypes.h"\
	"c:\mssdk\include\d3dvec.inl"\
	{$(INCLUDE)}"\d3d.h"\
	

"$(INTDIR)\fog.obj" : $(SOURCE) $(DEP_CPP_FOG_C) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\resource.h

!IF  "$(CFG)" == "MFCFog - Win32 Release"

!ELSEIF  "$(CFG)" == "MFCFog - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\stdafx.cpp
DEP_CPP_STDAF=\
	".\stdafx.h"\
	

"$(INTDIR)\stdafx.obj" : $(SOURCE) $(DEP_CPP_STDAF) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\d3dapp.cpp
DEP_CPP_D3DAPP=\
	"..\..\include\D3DEnum.h"\
	"..\..\include\D3DFrame.h"\
	"..\..\include\D3DUtil.h"\
	".\d3dapp.h"\
	".\stdafx.h"\
	"c:\mssdk\include\d3dcaps.h"\
	"c:\mssdk\include\d3dtypes.h"\
	"c:\mssdk\include\d3dvec.inl"\
	{$(INCLUDE)}"\d3d.h"\
	

"$(INTDIR)\d3dapp.obj" : $(SOURCE) $(DEP_CPP_D3DAPP) "$(INTDIR)"


# End Source File
# End Target
# End Project
################################################################################

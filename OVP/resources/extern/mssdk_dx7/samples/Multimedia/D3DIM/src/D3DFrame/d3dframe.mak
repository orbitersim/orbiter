# Microsoft Developer Studio Generated NMAKE File, Format Version 4.20
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

!IF "$(CFG)" == ""
CFG=d3dframe - Win32 Debug
!MESSAGE No configuration specified.  Defaulting to d3dframe - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "d3dframe - Win32 Release" && "$(CFG)" !=\
 "d3dframe - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "d3dframe.mak" CFG="d3dframe - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "d3dframe - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "d3dframe - Win32 Debug" (based on "Win32 (x86) Static Library")
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
# PROP Target_Last_Scanned "d3dframe - Win32 Debug"
CPP=cl.exe

!IF  "$(CFG)" == "d3dframe - Win32 Release"

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

ALL : "$(OUTDIR)\d3dframe.lib"

CLEAN : 
	-@erase "$(INTDIR)\d3dapp.obj"
	-@erase "$(INTDIR)\d3denum.obj"
	-@erase "$(INTDIR)\d3dfile.obj"
	-@erase "$(INTDIR)\d3dframe.obj"
	-@erase "$(INTDIR)\d3dmath.obj"
	-@erase "$(INTDIR)\d3dtextr.obj"
	-@erase "$(INTDIR)\d3dutil.obj"
	-@erase "$(OUTDIR)\d3dframe.lib"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /W3 /GX /O2 /I "..\..\include" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /c
CPP_PROJ=/nologo /ML /W3 /GX /O2 /I "..\..\include" /D "WIN32" /D "NDEBUG" /D\
 "_WINDOWS" /Fp"$(INTDIR)/d3dframe.pch" /YX /Fo"$(INTDIR)/" /c 
CPP_OBJS=.\Release/
CPP_SBRS=.\.
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/d3dframe.bsc" 
BSC32_SBRS= \
	
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo
LIB32_FLAGS=/nologo /out:"$(OUTDIR)/d3dframe.lib" 
LIB32_OBJS= \
	"$(INTDIR)\d3dapp.obj" \
	"$(INTDIR)\d3denum.obj" \
	"$(INTDIR)\d3dfile.obj" \
	"$(INTDIR)\d3dframe.obj" \
	"$(INTDIR)\d3dmath.obj" \
	"$(INTDIR)\d3dtextr.obj" \
	"$(INTDIR)\d3dutil.obj"

"$(OUTDIR)\d3dframe.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)
    $(LIB32) @<<
  $(LIB32_FLAGS) $(DEF_FLAGS) $(LIB32_OBJS)
<<

!ELSEIF  "$(CFG)" == "d3dframe - Win32 Debug"

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

ALL : "$(OUTDIR)\d3dframe.lib"

CLEAN : 
	-@erase "$(INTDIR)\d3dapp.obj"
	-@erase "$(INTDIR)\d3denum.obj"
	-@erase "$(INTDIR)\d3dfile.obj"
	-@erase "$(INTDIR)\d3dframe.obj"
	-@erase "$(INTDIR)\d3dmath.obj"
	-@erase "$(INTDIR)\d3dtextr.obj"
	-@erase "$(INTDIR)\d3dutil.obj"
	-@erase "$(INTDIR)\vc40.pdb"
	-@erase "$(OUTDIR)\d3dframe.lib"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /GX /Z7 /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /W3 /GX /Zi /Od /I "..\..\include" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /c
CPP_PROJ=/nologo /MLd /W3 /GX /Zi /Od /I "..\..\include" /D "WIN32" /D "_DEBUG"\
 /D "_WINDOWS" /Fp"$(INTDIR)/d3dframe.pch" /YX /Fo"$(INTDIR)/" /Fd"$(INTDIR)/"\
 /c 
CPP_OBJS=.\Debug/
CPP_SBRS=.\.
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/d3dframe.bsc" 
BSC32_SBRS= \
	
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo
LIB32_FLAGS=/nologo /out:"$(OUTDIR)/d3dframe.lib" 
LIB32_OBJS= \
	"$(INTDIR)\d3dapp.obj" \
	"$(INTDIR)\d3denum.obj" \
	"$(INTDIR)\d3dfile.obj" \
	"$(INTDIR)\d3dframe.obj" \
	"$(INTDIR)\d3dmath.obj" \
	"$(INTDIR)\d3dtextr.obj" \
	"$(INTDIR)\d3dutil.obj"

"$(OUTDIR)\d3dframe.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)
    $(LIB32) @<<
  $(LIB32_FLAGS) $(DEF_FLAGS) $(LIB32_OBJS)
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

# Name "d3dframe - Win32 Release"
# Name "d3dframe - Win32 Debug"

!IF  "$(CFG)" == "d3dframe - Win32 Release"

!ELSEIF  "$(CFG)" == "d3dframe - Win32 Debug"

!ENDIF 

################################################################################
# Begin Source File

SOURCE=.\d3denum.cpp
DEP_CPP_D3DEN=\
	"..\..\include\D3DEnum.h"\
	"..\..\include\D3DUtil.h"\
	"c:\mssdk\include\d3dcaps.h"\
	"c:\mssdk\include\d3dtypes.h"\
	"c:\mssdk\include\d3dvec.inl"\
	{$(INCLUDE)}"\d3d.h"\
	

"$(INTDIR)\d3denum.obj" : $(SOURCE) $(DEP_CPP_D3DEN) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\d3dframe.cpp
DEP_CPP_D3DFR=\
	"..\..\include\D3DFrame.h"\
	"..\..\include\D3DUtil.h"\
	"c:\mssdk\include\d3dcaps.h"\
	"c:\mssdk\include\d3dtypes.h"\
	"c:\mssdk\include\d3dvec.inl"\
	{$(INCLUDE)}"\d3d.h"\
	

"$(INTDIR)\d3dframe.obj" : $(SOURCE) $(DEP_CPP_D3DFR) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\d3dmath.cpp
DEP_CPP_D3DMA=\
	"..\..\include\D3DMath.h"\
	"c:\mssdk\include\d3dcaps.h"\
	"c:\mssdk\include\d3dtypes.h"\
	"c:\mssdk\include\d3dvec.inl"\
	{$(INCLUDE)}"\d3d.h"\
	

"$(INTDIR)\d3dmath.obj" : $(SOURCE) $(DEP_CPP_D3DMA) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\d3dtextr.cpp
DEP_CPP_D3DTE=\
	"..\..\include\D3DTextr.h"\
	"..\..\include\D3DUtil.h"\
	"c:\mssdk\include\d3dcaps.h"\
	"c:\mssdk\include\d3dtypes.h"\
	"c:\mssdk\include\d3dvec.inl"\
	{$(INCLUDE)}"\d3d.h"\
	

"$(INTDIR)\d3dtextr.obj" : $(SOURCE) $(DEP_CPP_D3DTE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\d3dutil.cpp
DEP_CPP_D3DUT=\
	"..\..\include\D3DUtil.h"\
	"c:\mssdk\include\d3dcaps.h"\
	"c:\mssdk\include\d3dtypes.h"\
	"c:\mssdk\include\d3dvec.inl"\
	{$(INCLUDE)}"\d3d.h"\
	

"$(INTDIR)\d3dutil.obj" : $(SOURCE) $(DEP_CPP_D3DUT) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\d3dfile.cpp
DEP_CPP_D3DFI=\
	"..\..\include\D3DFile.h"\
	"..\..\include\D3DMath.h"\
	"..\..\include\D3DTextr.h"\
	"..\..\include\D3DUtil.h"\
	"c:\mssdk\include\d3dcaps.h"\
	"c:\mssdk\include\d3dtypes.h"\
	"c:\mssdk\include\d3dvec.inl"\
	{$(INCLUDE)}"\d3d.h"\
	{$(INCLUDE)}"\dxfile.h"\
	{$(INCLUDE)}"\rmxfguid.h"\
	{$(INCLUDE)}"\rmxftmpl.h"\
	

"$(INTDIR)\d3dfile.obj" : $(SOURCE) $(DEP_CPP_D3DFI) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\d3dapp.cpp
DEP_CPP_D3DAP=\
	"..\..\include\D3DApp.h"\
	"..\..\include\D3DEnum.h"\
	"..\..\include\D3DFrame.h"\
	"..\..\include\D3DRes.h"\
	"..\..\include\D3DUtil.h"\
	"c:\mssdk\include\d3dcaps.h"\
	"c:\mssdk\include\d3dtypes.h"\
	"c:\mssdk\include\d3dvec.inl"\
	{$(INCLUDE)}"\d3d.h"\
	

"$(INTDIR)\d3dapp.obj" : $(SOURCE) $(DEP_CPP_D3DAP) "$(INTDIR)"


# End Source File
# End Target
# End Project
################################################################################

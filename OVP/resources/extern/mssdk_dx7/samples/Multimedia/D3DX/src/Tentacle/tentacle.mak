# Microsoft Developer Studio Generated NMAKE File, Format Version 4.20
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Application" 0x0101

!IF "$(CFG)" == ""
CFG=tentacle - Win32 Debug
!MESSAGE No configuration specified.  Defaulting to tentacle - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "tentacle - Win32 Release" && "$(CFG)" !=\
 "tentacle - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "tentacle.mak" CFG="tentacle - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "tentacle - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "tentacle - Win32 Debug" (based on "Win32 (x86) Application")
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
# PROP Target_Last_Scanned "tentacle - Win32 Debug"
MTL=mktyplib.exe
RSC=rc.exe
CPP=cl.exe

!IF  "$(CFG)" == "tentacle - Win32 Release"

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

ALL : "$(OUTDIR)\tentacle.exe"

CLEAN : 
	-@erase "$(INTDIR)\cframetimer.obj"
	-@erase "$(INTDIR)\clensflare.obj"
	-@erase "$(INTDIR)\ctentacle.obj"
	-@erase "$(INTDIR)\pch.obj"
	-@erase "$(INTDIR)\tentacle.obj"
	-@erase "$(INTDIR)\tentacle.pch"
	-@erase "$(INTDIR)\tentacle.res"
	-@erase "$(OUTDIR)\tentacle.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /c
CPP_PROJ=/nologo /ML /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS"\
 /Fp"$(INTDIR)/tentacle.pch" /YX /Fo"$(INTDIR)/" /c 
CPP_OBJS=.\Release/
CPP_SBRS=.\.
# ADD BASE MTL /nologo /D "NDEBUG" /win32
# ADD MTL /nologo /D "NDEBUG" /win32
MTL_PROJ=/nologo /D "NDEBUG" /win32 
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
RSC_PROJ=/l 0x409 /fo"$(INTDIR)/tentacle.res" /d "NDEBUG" 
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/tentacle.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib /nologo /subsystem:windows /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib winmm.lib ddraw.lib d3dx.lib /nologo /subsystem:windows /machine:I386
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib winmm.lib ddraw.lib\
 d3dx.lib /nologo /subsystem:windows /incremental:no\
 /pdb:"$(OUTDIR)/tentacle.pdb" /machine:I386 /out:"$(OUTDIR)/tentacle.exe" 
LINK32_OBJS= \
	"$(INTDIR)\cframetimer.obj" \
	"$(INTDIR)\clensflare.obj" \
	"$(INTDIR)\ctentacle.obj" \
	"$(INTDIR)\pch.obj" \
	"$(INTDIR)\tentacle.obj" \
	"$(INTDIR)\tentacle.res"

"$(OUTDIR)\tentacle.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "tentacle - Win32 Debug"

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

ALL : "$(OUTDIR)\tentacle.exe"

CLEAN : 
	-@erase "$(INTDIR)\cframetimer.obj"
	-@erase "$(INTDIR)\clensflare.obj"
	-@erase "$(INTDIR)\ctentacle.obj"
	-@erase "$(INTDIR)\pch.obj"
	-@erase "$(INTDIR)\tentacle.obj"
	-@erase "$(INTDIR)\tentacle.pch"
	-@erase "$(INTDIR)\tentacle.res"
	-@erase "$(INTDIR)\vc40.idb"
	-@erase "$(INTDIR)\vc40.pdb"
	-@erase "$(OUTDIR)\tentacle.exe"
	-@erase "$(OUTDIR)\tentacle.ilk"
	-@erase "$(OUTDIR)\tentacle.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /c
CPP_PROJ=/nologo /MLd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS"\
 /Fp"$(INTDIR)/tentacle.pch" /YX /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c 
CPP_OBJS=.\Debug/
CPP_SBRS=.\.
# ADD BASE MTL /nologo /D "_DEBUG" /win32
# ADD MTL /nologo /D "_DEBUG" /win32
MTL_PROJ=/nologo /D "_DEBUG" /win32 
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
RSC_PROJ=/l 0x409 /fo"$(INTDIR)/tentacle.res" /d "_DEBUG" 
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/tentacle.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib /nologo /subsystem:windows /debug /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib winmm.lib ddraw.lib d3dx.lib /nologo /subsystem:windows /debug /machine:I386
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib winmm.lib ddraw.lib\
 d3dx.lib /nologo /subsystem:windows /incremental:yes\
 /pdb:"$(OUTDIR)/tentacle.pdb" /debug /machine:I386\
 /out:"$(OUTDIR)/tentacle.exe" 
LINK32_OBJS= \
	"$(INTDIR)\cframetimer.obj" \
	"$(INTDIR)\clensflare.obj" \
	"$(INTDIR)\ctentacle.obj" \
	"$(INTDIR)\pch.obj" \
	"$(INTDIR)\tentacle.obj" \
	"$(INTDIR)\tentacle.res"

"$(OUTDIR)\tentacle.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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

# Name "tentacle - Win32 Release"
# Name "tentacle - Win32 Debug"

!IF  "$(CFG)" == "tentacle - Win32 Release"

!ELSEIF  "$(CFG)" == "tentacle - Win32 Debug"

!ENDIF 

################################################################################
# Begin Source File

SOURCE=.\tentacle.rc
DEP_RSC_TENTA=\
	".\directx.ico"\
	

"$(INTDIR)\tentacle.res" : $(SOURCE) $(DEP_RSC_TENTA) "$(INTDIR)"
   $(RSC) $(RSC_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=.\clensflare.cpp
DEP_CPP_CLENS=\
	"..\..\..\..\..\..\mssdk\include\d3dcaps.h"\
	"..\..\..\..\..\..\mssdk\include\d3dtypes.h"\
	"..\..\..\..\..\..\mssdk\include\d3dvec.inl"\
	"..\..\..\..\..\..\mssdk\include\d3dxcore.h"\
	"..\..\..\..\..\..\mssdk\include\d3dxerr.h"\
	"..\..\..\..\..\..\mssdk\include\d3dxmath.inl"\
	"..\..\..\..\..\..\mssdk\include\d3dxshapes.h"\
	"..\..\..\..\..\..\mssdk\include\d3dxsprite.h"\
	".\cframetimer.hpp"\
	".\clensflare.hpp"\
	".\ctentacle.hpp"\
	".\pch.hpp"\
	{$(INCLUDE)}"\d3d.h"\
	{$(INCLUDE)}"\d3dx.h"\
	{$(INCLUDE)}"\d3dxmath.h"\
	

!IF  "$(CFG)" == "tentacle - Win32 Release"

# ADD CPP /Yu"pch.hpp"

"$(INTDIR)\clensflare.obj" : $(SOURCE) $(DEP_CPP_CLENS) "$(INTDIR)"\
 "$(INTDIR)\tentacle.pch"
   $(CPP) /nologo /ML /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS"\
 /Fp"$(INTDIR)/tentacle.pch" /Yu"pch.hpp" /Fo"$(INTDIR)/" /c $(SOURCE)


!ELSEIF  "$(CFG)" == "tentacle - Win32 Debug"

# ADD CPP /Yu"pch.hpp"

"$(INTDIR)\clensflare.obj" : $(SOURCE) $(DEP_CPP_CLENS) "$(INTDIR)"\
 "$(INTDIR)\tentacle.pch"
   $(CPP) /nologo /MLd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS"\
 /Fp"$(INTDIR)/tentacle.pch" /Yu"pch.hpp" /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c\
 $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\ctentacle.cpp
DEP_CPP_CTENT=\
	"..\..\..\..\..\..\mssdk\include\d3dcaps.h"\
	"..\..\..\..\..\..\mssdk\include\d3dtypes.h"\
	"..\..\..\..\..\..\mssdk\include\d3dvec.inl"\
	"..\..\..\..\..\..\mssdk\include\d3dxcore.h"\
	"..\..\..\..\..\..\mssdk\include\d3dxerr.h"\
	"..\..\..\..\..\..\mssdk\include\d3dxmath.inl"\
	"..\..\..\..\..\..\mssdk\include\d3dxshapes.h"\
	"..\..\..\..\..\..\mssdk\include\d3dxsprite.h"\
	".\cframetimer.hpp"\
	".\clensflare.hpp"\
	".\ctentacle.hpp"\
	".\pch.hpp"\
	{$(INCLUDE)}"\d3d.h"\
	{$(INCLUDE)}"\d3dx.h"\
	{$(INCLUDE)}"\d3dxmath.h"\
	

!IF  "$(CFG)" == "tentacle - Win32 Release"

# ADD CPP /Yu"pch.hpp"

"$(INTDIR)\ctentacle.obj" : $(SOURCE) $(DEP_CPP_CTENT) "$(INTDIR)"\
 "$(INTDIR)\tentacle.pch"
   $(CPP) /nologo /ML /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS"\
 /Fp"$(INTDIR)/tentacle.pch" /Yu"pch.hpp" /Fo"$(INTDIR)/" /c $(SOURCE)


!ELSEIF  "$(CFG)" == "tentacle - Win32 Debug"

# ADD CPP /Yu"pch.hpp"

"$(INTDIR)\ctentacle.obj" : $(SOURCE) $(DEP_CPP_CTENT) "$(INTDIR)"\
 "$(INTDIR)\tentacle.pch"
   $(CPP) /nologo /MLd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS"\
 /Fp"$(INTDIR)/tentacle.pch" /Yu"pch.hpp" /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c\
 $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\pch.cpp
DEP_CPP_PCH_C=\
	"..\..\..\..\..\..\mssdk\include\d3dcaps.h"\
	"..\..\..\..\..\..\mssdk\include\d3dtypes.h"\
	"..\..\..\..\..\..\mssdk\include\d3dvec.inl"\
	"..\..\..\..\..\..\mssdk\include\d3dxcore.h"\
	"..\..\..\..\..\..\mssdk\include\d3dxerr.h"\
	"..\..\..\..\..\..\mssdk\include\d3dxmath.inl"\
	"..\..\..\..\..\..\mssdk\include\d3dxshapes.h"\
	"..\..\..\..\..\..\mssdk\include\d3dxsprite.h"\
	".\cframetimer.hpp"\
	".\clensflare.hpp"\
	".\ctentacle.hpp"\
	".\pch.hpp"\
	{$(INCLUDE)}"\d3d.h"\
	{$(INCLUDE)}"\d3dx.h"\
	{$(INCLUDE)}"\d3dxmath.h"\
	

!IF  "$(CFG)" == "tentacle - Win32 Release"

# ADD CPP /Yc"pch.hpp"

BuildCmds= \
	$(CPP) /nologo /ML /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS"\
 /Fp"$(INTDIR)/tentacle.pch" /Yc"pch.hpp" /Fo"$(INTDIR)/" /c $(SOURCE) \
	

"$(INTDIR)\pch.obj" : $(SOURCE) $(DEP_CPP_PCH_C) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\tentacle.pch" : $(SOURCE) $(DEP_CPP_PCH_C) "$(INTDIR)"
   $(BuildCmds)

!ELSEIF  "$(CFG)" == "tentacle - Win32 Debug"

# ADD CPP /Yc"pch.hpp"

BuildCmds= \
	$(CPP) /nologo /MLd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS"\
 /Fp"$(INTDIR)/tentacle.pch" /Yc"pch.hpp" /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c\
 $(SOURCE) \
	

"$(INTDIR)\pch.obj" : $(SOURCE) $(DEP_CPP_PCH_C) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\tentacle.pch" : $(SOURCE) $(DEP_CPP_PCH_C) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\resource.h

!IF  "$(CFG)" == "tentacle - Win32 Release"

!ELSEIF  "$(CFG)" == "tentacle - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\tentacle.cpp
DEP_CPP_TENTAC=\
	"..\..\..\..\..\..\mssdk\include\d3dcaps.h"\
	"..\..\..\..\..\..\mssdk\include\d3dtypes.h"\
	"..\..\..\..\..\..\mssdk\include\d3dvec.inl"\
	"..\..\..\..\..\..\mssdk\include\d3dxcore.h"\
	"..\..\..\..\..\..\mssdk\include\d3dxerr.h"\
	"..\..\..\..\..\..\mssdk\include\d3dxmath.inl"\
	"..\..\..\..\..\..\mssdk\include\d3dxshapes.h"\
	"..\..\..\..\..\..\mssdk\include\d3dxsprite.h"\
	".\cframetimer.hpp"\
	".\clensflare.hpp"\
	".\ctentacle.hpp"\
	".\pch.hpp"\
	{$(INCLUDE)}"\d3d.h"\
	{$(INCLUDE)}"\d3dx.h"\
	{$(INCLUDE)}"\d3dxmath.h"\
	

!IF  "$(CFG)" == "tentacle - Win32 Release"

# ADD CPP /Yu"pch.hpp"

"$(INTDIR)\tentacle.obj" : $(SOURCE) $(DEP_CPP_TENTAC) "$(INTDIR)"\
 "$(INTDIR)\tentacle.pch"
   $(CPP) /nologo /ML /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS"\
 /Fp"$(INTDIR)/tentacle.pch" /Yu"pch.hpp" /Fo"$(INTDIR)/" /c $(SOURCE)


!ELSEIF  "$(CFG)" == "tentacle - Win32 Debug"

# ADD CPP /Yu"pch.hpp"

"$(INTDIR)\tentacle.obj" : $(SOURCE) $(DEP_CPP_TENTAC) "$(INTDIR)"\
 "$(INTDIR)\tentacle.pch"
   $(CPP) /nologo /MLd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS"\
 /Fp"$(INTDIR)/tentacle.pch" /Yu"pch.hpp" /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c\
 $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\cframetimer.cpp
DEP_CPP_CFRAM=\
	"..\..\..\..\..\..\mssdk\include\d3dcaps.h"\
	"..\..\..\..\..\..\mssdk\include\d3dtypes.h"\
	"..\..\..\..\..\..\mssdk\include\d3dvec.inl"\
	"..\..\..\..\..\..\mssdk\include\d3dxcore.h"\
	"..\..\..\..\..\..\mssdk\include\d3dxerr.h"\
	"..\..\..\..\..\..\mssdk\include\d3dxmath.inl"\
	"..\..\..\..\..\..\mssdk\include\d3dxshapes.h"\
	"..\..\..\..\..\..\mssdk\include\d3dxsprite.h"\
	".\cframetimer.hpp"\
	".\clensflare.hpp"\
	".\ctentacle.hpp"\
	".\pch.hpp"\
	{$(INCLUDE)}"\d3d.h"\
	{$(INCLUDE)}"\d3dx.h"\
	{$(INCLUDE)}"\d3dxmath.h"\
	

!IF  "$(CFG)" == "tentacle - Win32 Release"

# ADD CPP /Yu"pch.hpp"

"$(INTDIR)\cframetimer.obj" : $(SOURCE) $(DEP_CPP_CFRAM) "$(INTDIR)"\
 "$(INTDIR)\tentacle.pch"
   $(CPP) /nologo /ML /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS"\
 /Fp"$(INTDIR)/tentacle.pch" /Yu"pch.hpp" /Fo"$(INTDIR)/" /c $(SOURCE)


!ELSEIF  "$(CFG)" == "tentacle - Win32 Debug"

# ADD CPP /Yu"pch.hpp"

"$(INTDIR)\cframetimer.obj" : $(SOURCE) $(DEP_CPP_CFRAM) "$(INTDIR)"\
 "$(INTDIR)\tentacle.pch"
   $(CPP) /nologo /MLd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS"\
 /Fp"$(INTDIR)/tentacle.pch" /Yu"pch.hpp" /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c\
 $(SOURCE)


!ENDIF 

# End Source File
# End Target
# End Project
################################################################################

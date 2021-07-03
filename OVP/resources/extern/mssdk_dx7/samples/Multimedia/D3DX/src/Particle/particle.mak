# Microsoft Developer Studio Generated NMAKE File, Format Version 4.20
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Application" 0x0101

!IF "$(CFG)" == ""
CFG=particle - Win32 Debug
!MESSAGE No configuration specified.  Defaulting to particle - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "particle - Win32 Release" && "$(CFG)" !=\
 "particle - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "particle.mak" CFG="particle - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "particle - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "particle - Win32 Debug" (based on "Win32 (x86) Application")
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
# PROP Target_Last_Scanned "particle - Win32 Debug"
CPP=cl.exe
MTL=mktyplib.exe
RSC=rc.exe

!IF  "$(CFG)" == "particle - Win32 Release"

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

ALL : "$(OUTDIR)\particle.exe"

CLEAN : 
	-@erase "$(INTDIR)\cframetimer.obj"
	-@erase "$(INTDIR)\cground.obj"
	-@erase "$(INTDIR)\cparticle.obj"
	-@erase "$(INTDIR)\particle.obj"
	-@erase "$(INTDIR)\particle.pch"
	-@erase "$(INTDIR)\particle.res"
	-@erase "$(INTDIR)\pch.obj"
	-@erase "$(OUTDIR)\particle.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /c
CPP_PROJ=/nologo /ML /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS"\
 /Fp"$(INTDIR)/particle.pch" /YX /Fo"$(INTDIR)/" /c 
CPP_OBJS=.\Release/
CPP_SBRS=.\.
# ADD BASE MTL /nologo /D "NDEBUG" /win32
# ADD MTL /nologo /D "NDEBUG" /win32
MTL_PROJ=/nologo /D "NDEBUG" /win32 
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
RSC_PROJ=/l 0x409 /fo"$(INTDIR)/particle.res" /d "NDEBUG" 
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/particle.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib /nologo /subsystem:windows /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib winmm.lib ddraw.lib d3dx.lib /nologo /subsystem:windows /machine:I386
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib winmm.lib ddraw.lib\
 d3dx.lib /nologo /subsystem:windows /incremental:no\
 /pdb:"$(OUTDIR)/particle.pdb" /machine:I386 /out:"$(OUTDIR)/particle.exe" 
LINK32_OBJS= \
	"$(INTDIR)\cframetimer.obj" \
	"$(INTDIR)\cground.obj" \
	"$(INTDIR)\cparticle.obj" \
	"$(INTDIR)\particle.obj" \
	"$(INTDIR)\particle.res" \
	"$(INTDIR)\pch.obj"

"$(OUTDIR)\particle.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "particle - Win32 Debug"

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

ALL : "$(OUTDIR)\particle.exe"

CLEAN : 
	-@erase "$(INTDIR)\cframetimer.obj"
	-@erase "$(INTDIR)\cground.obj"
	-@erase "$(INTDIR)\cparticle.obj"
	-@erase "$(INTDIR)\particle.obj"
	-@erase "$(INTDIR)\particle.pch"
	-@erase "$(INTDIR)\particle.res"
	-@erase "$(INTDIR)\pch.obj"
	-@erase "$(INTDIR)\vc40.idb"
	-@erase "$(INTDIR)\vc40.pdb"
	-@erase "$(OUTDIR)\particle.exe"
	-@erase "$(OUTDIR)\particle.ilk"
	-@erase "$(OUTDIR)\particle.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /c
CPP_PROJ=/nologo /MLd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS"\
 /Fp"$(INTDIR)/particle.pch" /YX /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c 
CPP_OBJS=.\Debug/
CPP_SBRS=.\.
# ADD BASE MTL /nologo /D "_DEBUG" /win32
# ADD MTL /nologo /D "_DEBUG" /win32
MTL_PROJ=/nologo /D "_DEBUG" /win32 
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
RSC_PROJ=/l 0x409 /fo"$(INTDIR)/particle.res" /d "_DEBUG" 
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/particle.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib /nologo /subsystem:windows /debug /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib winmm.lib ddraw.lib d3dx.lib /nologo /subsystem:windows /debug /machine:I386
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib winmm.lib ddraw.lib\
 d3dx.lib /nologo /subsystem:windows /incremental:yes\
 /pdb:"$(OUTDIR)/particle.pdb" /debug /machine:I386\
 /out:"$(OUTDIR)/particle.exe" 
LINK32_OBJS= \
	"$(INTDIR)\cframetimer.obj" \
	"$(INTDIR)\cground.obj" \
	"$(INTDIR)\cparticle.obj" \
	"$(INTDIR)\particle.obj" \
	"$(INTDIR)\particle.res" \
	"$(INTDIR)\pch.obj"

"$(OUTDIR)\particle.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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

# Name "particle - Win32 Release"
# Name "particle - Win32 Debug"

!IF  "$(CFG)" == "particle - Win32 Release"

!ELSEIF  "$(CFG)" == "particle - Win32 Debug"

!ENDIF 

################################################################################
# Begin Source File

SOURCE=.\resource.h

!IF  "$(CFG)" == "particle - Win32 Release"

!ELSEIF  "$(CFG)" == "particle - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\cground.cpp
DEP_CPP_CGROU=\
	"..\..\..\..\..\..\mssdk\include\d3dcaps.h"\
	"..\..\..\..\..\..\mssdk\include\d3dtypes.h"\
	"..\..\..\..\..\..\mssdk\include\d3dvec.inl"\
	"..\..\..\..\..\..\mssdk\include\d3dxcore.h"\
	"..\..\..\..\..\..\mssdk\include\d3dxerr.h"\
	"..\..\..\..\..\..\mssdk\include\d3dxmath.inl"\
	"..\..\..\..\..\..\mssdk\include\d3dxshapes.h"\
	"..\..\..\..\..\..\mssdk\include\d3dxsprite.h"\
	".\cframetimer.hpp"\
	".\cground.hpp"\
	".\cparticle.hpp"\
	".\pch.hpp"\
	{$(INCLUDE)}"\d3d.h"\
	{$(INCLUDE)}"\d3dx.h"\
	{$(INCLUDE)}"\d3dxmath.h"\
	

!IF  "$(CFG)" == "particle - Win32 Release"

# ADD CPP /Yu"pch.hpp"

"$(INTDIR)\cground.obj" : $(SOURCE) $(DEP_CPP_CGROU) "$(INTDIR)"\
 "$(INTDIR)\particle.pch"
   $(CPP) /nologo /ML /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS"\
 /Fp"$(INTDIR)/particle.pch" /Yu"pch.hpp" /Fo"$(INTDIR)/" /c $(SOURCE)


!ELSEIF  "$(CFG)" == "particle - Win32 Debug"

# ADD CPP /Yu"pch.hpp"

"$(INTDIR)\cground.obj" : $(SOURCE) $(DEP_CPP_CGROU) "$(INTDIR)"\
 "$(INTDIR)\particle.pch"
   $(CPP) /nologo /MLd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS"\
 /Fp"$(INTDIR)/particle.pch" /Yu"pch.hpp" /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c\
 $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\cparticle.cpp
DEP_CPP_CPART=\
	"..\..\..\..\..\..\mssdk\include\d3dcaps.h"\
	"..\..\..\..\..\..\mssdk\include\d3dtypes.h"\
	"..\..\..\..\..\..\mssdk\include\d3dvec.inl"\
	"..\..\..\..\..\..\mssdk\include\d3dxcore.h"\
	"..\..\..\..\..\..\mssdk\include\d3dxerr.h"\
	"..\..\..\..\..\..\mssdk\include\d3dxmath.inl"\
	"..\..\..\..\..\..\mssdk\include\d3dxshapes.h"\
	"..\..\..\..\..\..\mssdk\include\d3dxsprite.h"\
	".\cframetimer.hpp"\
	".\cground.hpp"\
	".\cparticle.hpp"\
	".\pch.hpp"\
	{$(INCLUDE)}"\d3d.h"\
	{$(INCLUDE)}"\d3dx.h"\
	{$(INCLUDE)}"\d3dxmath.h"\
	

!IF  "$(CFG)" == "particle - Win32 Release"

# ADD CPP /Yu"pch.hpp"

"$(INTDIR)\cparticle.obj" : $(SOURCE) $(DEP_CPP_CPART) "$(INTDIR)"\
 "$(INTDIR)\particle.pch"
   $(CPP) /nologo /ML /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS"\
 /Fp"$(INTDIR)/particle.pch" /Yu"pch.hpp" /Fo"$(INTDIR)/" /c $(SOURCE)


!ELSEIF  "$(CFG)" == "particle - Win32 Debug"

# ADD CPP /Yu"pch.hpp"

"$(INTDIR)\cparticle.obj" : $(SOURCE) $(DEP_CPP_CPART) "$(INTDIR)"\
 "$(INTDIR)\particle.pch"
   $(CPP) /nologo /MLd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS"\
 /Fp"$(INTDIR)/particle.pch" /Yu"pch.hpp" /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c\
 $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\particle.cpp
DEP_CPP_PARTI=\
	"..\..\..\..\..\..\mssdk\include\d3dcaps.h"\
	"..\..\..\..\..\..\mssdk\include\d3dtypes.h"\
	"..\..\..\..\..\..\mssdk\include\d3dvec.inl"\
	"..\..\..\..\..\..\mssdk\include\d3dxcore.h"\
	"..\..\..\..\..\..\mssdk\include\d3dxerr.h"\
	"..\..\..\..\..\..\mssdk\include\d3dxmath.inl"\
	"..\..\..\..\..\..\mssdk\include\d3dxshapes.h"\
	"..\..\..\..\..\..\mssdk\include\d3dxsprite.h"\
	".\cframetimer.hpp"\
	".\cground.hpp"\
	".\cparticle.hpp"\
	".\pch.hpp"\
	{$(INCLUDE)}"\d3d.h"\
	{$(INCLUDE)}"\d3dx.h"\
	{$(INCLUDE)}"\d3dxmath.h"\
	

!IF  "$(CFG)" == "particle - Win32 Release"

# ADD CPP /Yu"pch.hpp"

"$(INTDIR)\particle.obj" : $(SOURCE) $(DEP_CPP_PARTI) "$(INTDIR)"\
 "$(INTDIR)\particle.pch"
   $(CPP) /nologo /ML /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS"\
 /Fp"$(INTDIR)/particle.pch" /Yu"pch.hpp" /Fo"$(INTDIR)/" /c $(SOURCE)


!ELSEIF  "$(CFG)" == "particle - Win32 Debug"

# ADD CPP /Yu"pch.hpp"

"$(INTDIR)\particle.obj" : $(SOURCE) $(DEP_CPP_PARTI) "$(INTDIR)"\
 "$(INTDIR)\particle.pch"
   $(CPP) /nologo /MLd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS"\
 /Fp"$(INTDIR)/particle.pch" /Yu"pch.hpp" /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c\
 $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\particle.rc
DEP_RSC_PARTIC=\
	".\directx.ico"\
	

"$(INTDIR)\particle.res" : $(SOURCE) $(DEP_RSC_PARTIC) "$(INTDIR)"
   $(RSC) $(RSC_PROJ) $(SOURCE)


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
	".\cground.hpp"\
	".\cparticle.hpp"\
	".\pch.hpp"\
	{$(INCLUDE)}"\d3d.h"\
	{$(INCLUDE)}"\d3dx.h"\
	{$(INCLUDE)}"\d3dxmath.h"\
	

!IF  "$(CFG)" == "particle - Win32 Release"

# ADD CPP /Yc"pch.hpp"

BuildCmds= \
	$(CPP) /nologo /ML /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS"\
 /Fp"$(INTDIR)/particle.pch" /Yc"pch.hpp" /Fo"$(INTDIR)/" /c $(SOURCE) \
	

"$(INTDIR)\pch.obj" : $(SOURCE) $(DEP_CPP_PCH_C) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\particle.pch" : $(SOURCE) $(DEP_CPP_PCH_C) "$(INTDIR)"
   $(BuildCmds)

!ELSEIF  "$(CFG)" == "particle - Win32 Debug"

# ADD CPP /Yc"pch.hpp"

BuildCmds= \
	$(CPP) /nologo /MLd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS"\
 /Fp"$(INTDIR)/particle.pch" /Yc"pch.hpp" /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c\
 $(SOURCE) \
	

"$(INTDIR)\pch.obj" : $(SOURCE) $(DEP_CPP_PCH_C) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\particle.pch" : $(SOURCE) $(DEP_CPP_PCH_C) "$(INTDIR)"
   $(BuildCmds)

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
	".\cground.hpp"\
	".\cparticle.hpp"\
	".\pch.hpp"\
	{$(INCLUDE)}"\d3d.h"\
	{$(INCLUDE)}"\d3dx.h"\
	{$(INCLUDE)}"\d3dxmath.h"\
	

!IF  "$(CFG)" == "particle - Win32 Release"

# ADD CPP /Yu"pch.hpp"

"$(INTDIR)\cframetimer.obj" : $(SOURCE) $(DEP_CPP_CFRAM) "$(INTDIR)"\
 "$(INTDIR)\particle.pch"
   $(CPP) /nologo /ML /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS"\
 /Fp"$(INTDIR)/particle.pch" /Yu"pch.hpp" /Fo"$(INTDIR)/" /c $(SOURCE)


!ELSEIF  "$(CFG)" == "particle - Win32 Debug"

# ADD CPP /Yu"pch.hpp"

"$(INTDIR)\cframetimer.obj" : $(SOURCE) $(DEP_CPP_CFRAM) "$(INTDIR)"\
 "$(INTDIR)\particle.pch"
   $(CPP) /nologo /MLd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS"\
 /Fp"$(INTDIR)/particle.pch" /Yu"pch.hpp" /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c\
 $(SOURCE)


!ENDIF 

# End Source File
# End Target
# End Project
################################################################################

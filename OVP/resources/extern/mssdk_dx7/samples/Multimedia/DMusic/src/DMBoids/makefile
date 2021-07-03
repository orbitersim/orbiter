proj=DMBoids

!if "$(nodebug)" == "1"
OBJ_DIR = Retail
!else
OBJ_DIR = Debug
!endif

!include "$(DXSDKROOT)\include\dxsdk.inc"

mycflags=

OBJ_LIST = $(OBJ_DIR)\boids.obj $(OBJ_DIR)\d3dtex.obj $(OBJ_DIR)\d3dutils.obj $(OBJ_DIR)\d3dwin.obj \
	$(OBJ_DIR)\debug.obj $(OBJ_DIR)\drvmgr.obj $(OBJ_DIR)\flock.obj $(OBJ_DIR)\gull.obj \
	$(OBJ_DIR)\input.obj $(OBJ_DIR)\music.obj $(OBJ_DIR)\winmain.obj $(OBJ_DIR)\winproc.obj

all: mkdir $(OBJ_DIR)\$(proj).exe

$(OBJ_DIR)\boids.obj:  boids.cpp
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\boids.obj boids.cpp

$(OBJ_DIR)\d3dtex.obj:  d3dtex.cpp
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\d3dtex.obj d3dtex.cpp

$(OBJ_DIR)\d3dutils.obj:  d3dutils.cpp
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\d3dutils.obj d3dutils.cpp

$(OBJ_DIR)\d3dwin.obj:  d3dwin.cpp
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\d3dwin.obj d3dwin.cpp

$(OBJ_DIR)\debug.obj:  debug.cpp
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\debug.obj debug.cpp

$(OBJ_DIR)\drvmgr.obj:  drvmgr.cpp
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\drvmgr.obj drvmgr.cpp

$(OBJ_DIR)\flock.obj:  flock.cpp
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\flock.obj flock.cpp

$(OBJ_DIR)\gull.obj:  gull.cpp
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\gull.obj gull.cpp

$(OBJ_DIR)\input.obj:  input.cpp
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\input.obj input.cpp

$(OBJ_DIR)\music.obj:  music.cpp
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\music.obj music.cpp

$(OBJ_DIR)\winmain.obj:  winmain.cpp
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\winmain.obj winmain.cpp

$(OBJ_DIR)\winproc.obj:  winproc.cpp
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\winproc.obj winproc.cpp

$(OBJ_DIR)\$(proj).res: $(proj).rc resource.h
   $(rc) $(rcflags) $(OBJ_DIR)\$(proj).res $(proj).rc

$(OBJ_DIR)\$(proj).exe: $(OBJ_LIST) $(OBJ_DIR)\$(proj).res
    $(link) $(linkdebug) $(linkflags) $(OBJ_LIST) $(borobjs) $(commas) $(outexe)$(OBJ_DIR)\$(proj).exe $(commas) $(commas) $(deflibs) ddraw.lib d3dim.lib dinput.lib dxguid.lib $(commas) $(commas) $(OBJ_DIR)\$(proj).res

mkdir:
    @if not exist $(OBJ_DIR)\NUL md $(OBJ_DIR)
    
# Rules for cleaning out those old files
clean:
    -@echo y | del Retail
    -@echo y | del Debug
    -@rd Retail
    -@rd Debug

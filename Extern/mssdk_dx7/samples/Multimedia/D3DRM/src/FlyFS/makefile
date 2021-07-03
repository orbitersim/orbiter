proj=flyfs

!if "$(nodebug)" == "1"
OBJ_DIR = Retail
!else
OBJ_DIR = Debug
!endif

!include "$(DXSDKROOT)\include\dxsdk.inc"

mycflags=

OBJ_LIST = $(OBJ_DIR)\ddcalls.obj $(OBJ_DIR)\d3dcalls.obj $(OBJ_DIR)\d3dapp.obj $(OBJ_DIR)\fly.obj $(OBJ_DIR)\rmfull.obj $(OBJ_DIR)\misc.obj $(OBJ_DIR)\rmstats.obj $(OBJ_DIR)\texture.obj

all: mkdir $(OBJ_DIR)\$(proj).exe

$(OBJ_DIR)\d3dapp.obj:  d3dapp.c
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\d3dapp.obj d3dapp.c

$(OBJ_DIR)\d3dcalls.obj:  d3dcalls.c
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\d3dcalls.obj d3dcalls.c

$(OBJ_DIR)\ddcalls.obj:  ddcalls.c
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\ddcalls.obj ddcalls.c

$(OBJ_DIR)\fly.obj:  fly.cpp
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\fly.obj fly.cpp

$(OBJ_DIR)\misc.obj:  misc.c
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\misc.obj misc.c

$(OBJ_DIR)\rmfull.obj:  rmfull.cpp
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\rmfull.obj rmfull.cpp

$(OBJ_DIR)\rmstats.obj:  rmstats.cpp
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\rmstats.obj rmstats.cpp

$(OBJ_DIR)\texture.obj:  texture.c
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\texture.obj texture.c

$(OBJ_DIR)\rmfull.res: rmfull.rc resource.h
   $(rc) $(rcflags) $(OBJ_DIR)\rmfull.res rmfull.rc

$(OBJ_DIR)\$(proj).exe: $(OBJ_LIST) $(OBJ_DIR)\rmfull.res
    $(link) $(linkdebug) $(linkflags) $(OBJ_LIST) $(borobjs) $(commas) $(outexe)$(OBJ_DIR)\$(proj).exe $(commas) $(commas) $(deflibs) ddraw.lib d3drm.lib $(commas) $(commas) $(OBJ_DIR)\rmfull.res

mkdir:
    @if not exist $(OBJ_DIR)\NUL md $(OBJ_DIR)
    
# Rules for cleaning out those old files
clean:
    -@echo y | del Retail
    -@echo y | del Debug
    -@rd Retail
    -@rd Debug

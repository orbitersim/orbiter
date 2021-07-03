proj=3DMusic

!if "$(nodebug)" == "1"
OBJ_DIR = Retail
!else
OBJ_DIR = Debug
!endif

!include "$(DXSDKROOT)\include\dxsdk.inc"

mycflags=

OBJ_LIST = $(OBJ_DIR)\dsutil3d.obj $(OBJ_DIR)\3dsound.obj $(OBJ_DIR)\helper.obj $(OBJ_DIR)\sound.obj

all: mkdir $(OBJ_DIR)\$(proj).exe

$(OBJ_DIR)\3dsound.obj:  3dsound.cpp
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\3dsound.obj 3dsound.cpp

$(OBJ_DIR)\dsutil3d.obj:  dsutil3d.c
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\dsutil3d.obj dsutil3d.c

$(OBJ_DIR)\helper.obj:  helper.cpp
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\helper.obj helper.cpp

$(OBJ_DIR)\sound.obj:  sound.cpp
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\sound.obj sound.cpp

$(OBJ_DIR)\$(proj).res: script1.rc resource.h
   $(rc) $(rcflags) $(OBJ_DIR)\$(proj).res script1.rc

$(OBJ_DIR)\$(proj).exe: $(OBJ_LIST) $(OBJ_DIR)\$(proj).res
    $(link) $(linkdebug) $(linkflags) $(OBJ_LIST) $(borobjs) $(commas) $(outexe)$(OBJ_DIR)\$(proj).exe $(commas) $(commas) $(deflibs) dsound.lib dxguid.lib $(commas) $(commas) $(OBJ_DIR)\$(proj).res

mkdir:
    @if not exist $(OBJ_DIR)\NUL md $(OBJ_DIR)
    
# Rules for cleaning out those old files
clean:
    -@echo y | del Retail
    -@echo y | del Debug
    -@rd Retail
    -@rd Debug

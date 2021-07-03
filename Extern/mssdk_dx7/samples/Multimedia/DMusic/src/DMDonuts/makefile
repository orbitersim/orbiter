proj=dmdonuts

!if "$(nodebug)" == "1"
OBJ_DIR = Retail
!else
OBJ_DIR = Debug
!endif

!include "$(DXSDKROOT)\include\dxsdk.inc"

mycflags=

OBJ_LIST = $(OBJ_DIR)\donuts.obj $(OBJ_DIR)\ddutil.obj $(OBJ_DIR)\dsutil.obj $(OBJ_DIR)\input.obj

all: mkdir $(OBJ_DIR)\$(proj).exe

$(OBJ_DIR)\donuts.obj:  donuts.cpp
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\donuts.obj donuts.cpp

$(OBJ_DIR)\ddutil.obj:  ddutil.cpp
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\ddutil.obj ddutil.cpp

$(OBJ_DIR)\dsutil.obj:  dsutil.cpp
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\dsutil.obj dsutil.cpp

$(OBJ_DIR)\input.obj:  input.cpp
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\input.obj input.cpp

$(OBJ_DIR)\$(proj).res: donuts.rc resource.h
   $(rc) $(rcflags) $(OBJ_DIR)\$(proj).res donuts.rc

$(OBJ_DIR)\$(proj).exe: $(OBJ_LIST) $(OBJ_DIR)\$(proj).res
    $(link) $(linkdebug) $(linkflags) $(OBJ_LIST) $(borobjs) $(commas) $(outexe)$(OBJ_DIR)\$(proj).exe $(commas) $(commas) $(deflibs) ddraw.lib dinput.lib dsound.lib $(commas) $(commas) $(OBJ_DIR)\$(proj).res

mkdir:
    @if not exist $(OBJ_DIR)\NUL md $(OBJ_DIR)
    
# Rules for cleaning out those old files
clean:
    -@echo y | del Retail
    -@echo y | del Debug
    -@rd Retail
    -@rd Debug

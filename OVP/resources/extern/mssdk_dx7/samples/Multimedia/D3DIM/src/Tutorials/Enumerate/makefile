proj=enumerate

!if "$(nodebug)" == "1"
OBJ_DIR = Retail
!else
OBJ_DIR = Debug
!endif

!include "$(DXSDKROOT)\include\dxsdk.inc"

OBJ_LIST = $(OBJ_DIR)\$(proj).obj $(OBJ_DIR)\render.obj $(OBJ_DIR)\winmain.obj

all: mkdir $(OBJ_DIR)\$(proj).exe

$(OBJ_DIR)\$(proj).obj:  $(proj).cpp
    $(cc) $(cdebug) $(cflags) $(outobj)$(OBJ_DIR)\$(proj).obj $(proj).cpp

$(OBJ_DIR)\render.obj:  render.cpp
    $(cc) $(cdebug) $(cflags) $(outobj)$(OBJ_DIR)\render.obj render.cpp

$(OBJ_DIR)\winmain.obj:  winmain.cpp
    $(cc) $(cdebug) $(cflags) $(outobj)$(OBJ_DIR)\winmain.obj winmain.cpp

$(OBJ_DIR)\winmain.res: winmain.rc resource.h
   $(rc) $(rcflags) $(OBJ_DIR)\winmain.res winmain.rc

$(OBJ_DIR)\$(proj).exe: $(OBJ_LIST) $(OBJ_DIR)\winmain.res
    $(link) $(linkdebug) $(linkflags) $(OBJ_LIST) $(borobjs) $(commas) $(outexe)$(OBJ_DIR)\$(proj).exe $(commas) $(commas) $(deflibs) ddraw.lib dxguid.lib $(commas) $(commas) $(OBJ_DIR)\winmain.res

mkdir:
    @if not exist $(OBJ_DIR)\NUL md $(OBJ_DIR)
    
# Rules for cleaning out those old files
clean:
    -@echo y | del Retail
    -@echo y | del Debug
    -@rd Retail
    -@rd Debug

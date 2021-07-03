proj=musiclines

!if "$(nodebug)" == "1"
OBJ_DIR = Retail
!else
OBJ_DIR = Debug
!endif

!include "$(DXSDKROOT)\include\dxsdk.inc"

mycflags=

OBJ_LIST = $(OBJ_DIR)\debug.obj $(OBJ_DIR)\dmhelper.obj $(OBJ_DIR)\mlai.obj $(OBJ_DIR)\mlbmpsrf.obj $(OBJ_DIR)\mlgame.obj $(OBJ_DIR)\mlinput.obj $(OBJ_DIR)\mllocalplayer.obj $(OBJ_DIR)\mlmain.obj $(OBJ_DIR)\mlmusic.obj $(OBJ_DIR)\mloptpal.obj $(OBJ_DIR)\mlrender.obj

all: mkdir $(OBJ_DIR)\$(proj).exe

$(OBJ_DIR)\debug.obj:  debug.cpp
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\debug.obj debug.cpp

$(OBJ_DIR)\dmhelper.obj:  dmhelper.cpp
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\dmhelper.obj dmhelper.cpp

$(OBJ_DIR)\mlai.obj:  mlai.cpp
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\mlai.obj mlai.cpp

$(OBJ_DIR)\mlbmpsrf.obj:  mlbmpsrf.cpp
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\mlbmpsrf.obj mlbmpsrf.cpp

$(OBJ_DIR)\mlgame.obj:  mlgame.cpp
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\mlgame.obj mlgame.cpp

$(OBJ_DIR)\mlinput.obj:  mlinput.cpp
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\mlinput.obj mlinput.cpp

$(OBJ_DIR)\mllocalplayer.obj:  mllocalplayer.cpp
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\mllocalplayer.obj mllocalplayer.cpp

$(OBJ_DIR)\mlmain.obj:  mlmain.cpp
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\mlmain.obj mlmain.cpp

$(OBJ_DIR)\mlmusic.obj:  mlmusic.cpp
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\mlmusic.obj mlmusic.cpp

$(OBJ_DIR)\mloptpal.obj:  mloptpal.cpp
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\mloptpal.obj mloptpal.cpp

$(OBJ_DIR)\mlrender.obj:  mlrender.cpp
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\mlrender.obj mlrender.cpp

$(OBJ_DIR)\$(proj).res: $(proj).rc resource.h
   $(rc) $(rcflags) $(OBJ_DIR)\$(proj).res $(proj).rc

$(OBJ_DIR)\$(proj).exe: $(OBJ_LIST) $(OBJ_DIR)\$(proj).res
    $(link) $(linkdebug) $(linkflags) $(OBJ_LIST) $(borobjs) $(commas) $(outexe)$(OBJ_DIR)\$(proj).exe $(commas) $(commas) $(deflibs) dxguid.lib ddraw.lib dinput.lib uuid.lib $(commas) $(commas) $(OBJ_DIR)\$(proj).res

mkdir:
    @if not exist $(OBJ_DIR)\NUL md $(OBJ_DIR)
    
# Rules for cleaning out those old files
clean:
    -@echo y | del Retail
    -@echo y | del Debug
    -@rd Retail
    -@rd Debug

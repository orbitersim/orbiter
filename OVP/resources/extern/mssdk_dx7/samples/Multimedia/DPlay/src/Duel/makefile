proj=Duel

!if "$(nodebug)" == "1"
OBJ_DIR = Retail
!else
OBJ_DIR = Debug
!endif

!include "$(DXSDKROOT)\include\dxsdk.inc"

!IFNDEF BORLAND
deflibs = $(deflibs) comctl32.lib
!ENDIF

mycflags=

OBJ_LIST = $(OBJ_DIR)\duel.obj $(OBJ_DIR)\dputil.obj $(OBJ_DIR)\ddutil.obj $(OBJ_DIR)\dsutil.obj \
        $(OBJ_DIR)\gameproc.obj $(OBJ_DIR)\gfx.obj $(OBJ_DIR)\diutil.obj $(OBJ_DIR)\lobby.obj \
        $(OBJ_DIR)\util.obj $(OBJ_DIR)\dpconnect.obj

all: mkdir $(OBJ_DIR)\$(proj).exe

$(OBJ_DIR)\duel.obj:  duel.cpp
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\duel.obj duel.cpp

$(OBJ_DIR)\dputil.obj:  dputil.cpp
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\dputil.obj dputil.cpp

$(OBJ_DIR)\dpconnect.obj:  dpconnect.cpp
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\dpconnect.obj dpconnect.cpp

$(OBJ_DIR)\ddutil.obj:  ddutil.cpp
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\ddutil.obj ddutil.cpp

$(OBJ_DIR)\dsutil.obj:  dsutil.cpp
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\dsutil.obj dsutil.cpp

$(OBJ_DIR)\gameproc.obj:  gameproc.cpp
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\gameproc.obj gameproc.cpp

$(OBJ_DIR)\gfx.obj:  gfx.cpp
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\gfx.obj gfx.cpp

$(OBJ_DIR)\diutil.obj:  diutil.cpp
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\diutil.obj diutil.cpp

$(OBJ_DIR)\lobby.obj:  lobby.cpp
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\lobby.obj lobby.cpp

$(OBJ_DIR)\util.obj:  util.cpp
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\util.obj util.cpp

$(OBJ_DIR)\$(proj).res: duel.rc resource.h
   $(rc) $(rcflags) $(OBJ_DIR)\$(proj).res duel.rc

$(OBJ_DIR)\$(proj).exe: $(OBJ_LIST) $(OBJ_DIR)\$(proj).res
    $(link) $(linkdebug) $(linkflags) $(OBJ_LIST) $(borobjs) $(commas) $(outexe)$(OBJ_DIR)\$(proj).exe $(commas) $(commas) $(deflibs) ddraw.lib dinput.lib dplayx.lib dsound.lib $(commas) $(commas) $(OBJ_DIR)\$(proj).res

mkdir:
    @if not exist $(OBJ_DIR)\NUL md $(OBJ_DIR)
    
# Rules for cleaning out those old files
clean:
    -@echo y | del Retail
    -@echo y | del Debug
    -@rd Retail
    -@rd Debug

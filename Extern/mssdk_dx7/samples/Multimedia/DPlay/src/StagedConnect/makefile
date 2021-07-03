proj=StagedConnect

!if "$(nodebug)" == "1"
OBJ_DIR = Retail
!else
OBJ_DIR = Debug
!endif

!include "$(DXSDKROOT)\include\dxsdk.inc"

!IFNDEF BORLAND
deflibs = $(deflibs) msacm32.lib
!ENDIF

mycflags=

OBJ_LIST = $(OBJ_DIR)\WinMain.obj $(OBJ_DIR)\DPConnect.obj $(OBJ_DIR)\DPStage.obj 

all: mkdir $(OBJ_DIR)\$(proj).exe

$(OBJ_DIR)\winmain.obj: winmain.cpp
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\winmain.obj winmain.cpp
    
$(OBJ_DIR)\dpconnect.obj: dpconnect.cpp
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\dpconnect.obj dpconnect.cpp

$(OBJ_DIR)\dpstage.obj: dpstage.cpp
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\dpstage.obj dpstage.cpp

$(OBJ_DIR)\$(proj).res: winmain.rc resource.h
   $(rc) $(rcflags) $(OBJ_DIR)\$(proj).res winmain.rc

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

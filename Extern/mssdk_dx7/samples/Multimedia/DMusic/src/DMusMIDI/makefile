proj=DMusMIDI

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

OBJ_LIST = $(OBJ_DIR)\debug.obj $(OBJ_DIR)\mainwnd.obj $(OBJ_DIR)\midiplyr.obj $(OBJ_DIR)\timewnd.obj \
           $(OBJ_DIR)\uiutils.obj

all: mkdir $(OBJ_DIR)\$(proj).exe

$(OBJ_DIR)\debug.obj:  debug.c
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\debug.obj debug.c

$(OBJ_DIR)\mainwnd.obj:  mainwnd.c
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\mainwnd.obj mainwnd.c

$(OBJ_DIR)\midiplyr.obj:  midiplyr.c
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\midiplyr.obj midiplyr.c

$(OBJ_DIR)\timewnd.obj:  timewnd.c
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\timewnd.obj timewnd.c

$(OBJ_DIR)\uiutils.obj:  uiutils.c
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\uiutils.obj uiutils.c

$(OBJ_DIR)\$(proj).res: midiplyr.rc resource.h
   $(rc) $(rcflags) $(OBJ_DIR)\$(proj).res midiplyr.rc

$(OBJ_DIR)\$(proj).exe: $(OBJ_LIST) $(OBJ_DIR)\$(proj).res
    $(link) $(linkdebug) $(linkflags) $(OBJ_LIST) $(borobjs) $(commas) $(outexe)$(OBJ_DIR)\$(proj).exe $(commas) $(commas) $(deflibs) dxguid.lib $(commas) $(commas) $(OBJ_DIR)\$(proj).res

mkdir:
    @if not exist $(OBJ_DIR)\NUL md $(OBJ_DIR)
    
# Rules for cleaning out those old files
clean:
    -@echo y | del Retail
    -@echo y | del Debug
    -@rd Retail
    -@rd Debug

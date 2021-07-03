proj=Donuts

INCLUDE=$(INCLUDE);$(DXSDKROOT)\samples\multimedia\ddraw\include;

!if "$(nodebug)" == "1"
OBJ_DIR = Retail
!else
OBJ_DIR = Debug
!endif

!include "$(DXSDKROOT)\include\dxsdk.inc"

!IFDEF BORLAND
mycflags=/DUSE_DSOUND
!ELSE
mycflags=/D "USE_DSOUND"
!ENDIF

OBJ_LIST = $(OBJ_DIR)\$(proj).obj $(OBJ_DIR)\ddutil.obj $(OBJ_DIR)\dsutil.obj $(OBJ_DIR)\input.obj

all: mkdir $(OBJ_DIR)\$(proj).exe

$(OBJ_DIR)\$(proj).obj:  $(proj).c
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\$(proj).obj $(proj).c

$(OBJ_DIR)\ddutil.obj:  ddutil.cpp
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\ddutil.obj ddutil.cpp

$(OBJ_DIR)\dsutil.obj:  dsutil.c
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\dsutil.obj dsutil.c

$(OBJ_DIR)\input.obj:  input.c
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\input.obj input.c

$(OBJ_DIR)\$(proj).res: $(proj).rc resource.h
   $(rc) $(rcflags) $(OBJ_DIR)\$(proj).res $(proj).rc

$(OBJ_DIR)\$(proj).exe: $(OBJ_LIST) $(OBJ_DIR)\$(proj).res
    $(link) $(linkdebug) $(linkflags) $(OBJ_LIST) $(borobjs) $(commas) $(outexe)$(OBJ_DIR)\$(proj).exe $(commas) $(commas) $(deflibs) dsound.lib ddraw.lib dinput.lib $(commas) $(commas) $(OBJ_DIR)\$(proj).res

mkdir:
    @if not exist $(OBJ_DIR)\NUL md $(OBJ_DIR)
    
# Rules for cleaning out those old files
clean:
    -@echo y | del Retail
    -@echo y | del Debug
    -@rd Retail
    -@rd Debug

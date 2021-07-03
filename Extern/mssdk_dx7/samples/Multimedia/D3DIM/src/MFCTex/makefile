proj=mfctex

MFC=1

INCLUDE=$(INCLUDE);$(DXSDKROOT)\samples\multimedia\d3dim\include;

!if "$(nodebug)" == "1"
OBJ_DIR = Retail
!else
OBJ_DIR = Debug
!endif

!include "$(DXSDKROOT)\include\dxsdk.inc"

mycflags=

OBJ_LIST = $(OBJ_DIR)\$(proj).obj $(OBJ_DIR)\mtexture.obj $(OBJ_DIR)\mfcform.obj

all: mkdir $(OBJ_DIR)\$(proj).exe

$(OBJ_DIR)\$(proj).obj:  $(proj).cpp
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\$(proj).obj $(proj).cpp

$(OBJ_DIR)\mtexture.obj:  mtexture.cpp
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\mtexture.obj mtexture.cpp

$(OBJ_DIR)\mfcform.obj:  mfcform.cpp
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\mfcform.obj mfcform.cpp

$(OBJ_DIR)\winmain.res: winmain.rc resource.h
   $(rc) $(rcflags) $(OBJ_DIR)\winmain.res winmain.rc

$(OBJ_DIR)\$(proj).exe: $(OBJ_LIST) $(OBJ_DIR)\winmain.res
    $(link) $(linkdebug) $(linkflags) $(OBJ_LIST) $(borobjs) $(commas) $(outexe)$(OBJ_DIR)\$(proj).exe $(commas) $(commas) $(deflibs) $(d3dframe) ddraw.lib dxguid.lib $(commas) $(commas) $(OBJ_DIR)\winmain.res

mkdir:
    @if not exist $(OBJ_DIR)\NUL md $(OBJ_DIR)
    
# Rules for cleaning out those old files
clean:
    -@echo y | del Retail
    -@echo y | del Debug
    -@rd Retail
    -@rd Debug

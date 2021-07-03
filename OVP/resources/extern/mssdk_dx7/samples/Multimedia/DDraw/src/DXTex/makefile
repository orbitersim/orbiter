proj=DXTex

MFC=1

INCLUDE=$(INCLUDE);$(DXSDKROOT)\samples\multimedia\d3dim\include;

!if "$(nodebug)" == "1"
OBJ_DIR = Retail
!else
OBJ_DIR = Debug
!endif

!include "$(DXSDKROOT)\include\dxsdk.inc"

mycflags=

OBJ_LIST = $(OBJ_DIR)\DXTex.obj $(OBJ_DIR)\ChildFrm.obj $(OBJ_DIR)\MainFrm.obj $(OBJ_DIR)\DXTexDoc.obj $(OBJ_DIR)\DXTexView.obj

all: mkdir $(OBJ_DIR)\$(proj).exe

$(OBJ_DIR)\DXTex.obj:  DXTex.cpp
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\DXTex.obj DXTex.cpp

$(OBJ_DIR)\ChildFrm.obj:  ChildFrm.cpp
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\ChildFrm.obj ChildFrm.cpp

$(OBJ_DIR)\MainFrm.obj:  MainFrm.cpp
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\MainFrm.obj MainFrm.cpp

$(OBJ_DIR)\DXTexDoc.obj:  DXTexDoc.cpp
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\DXTexDoc.obj DXTexDoc.cpp

$(OBJ_DIR)\DXTexView.obj:  DXTexView.cpp
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\DXTexView.obj DXTexView.cpp

$(OBJ_DIR)\DXTex.res: DXTex.rc resource.h
   $(rc) $(rcflags) $(OBJ_DIR)\DXTex.res DXTex.rc

$(OBJ_DIR)\$(proj).exe: $(OBJ_LIST) $(OBJ_DIR)\DXTex.res
    $(link) $(linkdebug) $(linkflags) $(OBJ_LIST) $(borobjs) $(commas) $(outexe)$(OBJ_DIR)\$(proj).exe $(commas) $(commas) $(deflibs) ddraw.lib dxguid.lib version.lib $(commas) $(commas) $(OBJ_DIR)\DXTex.res

mkdir:
    @if not exist $(OBJ_DIR)\NUL md $(OBJ_DIR)
    
# Rules for cleaning out those old files
clean:
    -@echo y | del Retail
    -@echo y | del Debug
    -@rd Retail
    -@rd Debug

proj=mfcfog

MFC=1

INCLUDE=$(INCLUDE);$(DXSDKROOT)\samples\multimedia\d3dim\include;

!if "$(nodebug)" == "1"
OBJ_DIR = Retail
!else
OBJ_DIR = Debug
!endif

!include "$(DXSDKROOT)\include\dxsdk.inc"

mycflags=

OBJ_LIST = $(OBJ_DIR)\d3dapp.obj $(OBJ_DIR)\fog.obj

all: mkdir $(OBJ_DIR)\$(proj).exe

$(OBJ_DIR)\d3dapp.obj:  d3dapp.cpp
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\d3dapp.obj d3dapp.cpp

$(OBJ_DIR)\fog.obj:  fog.cpp
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\fog.obj fog.cpp

$(OBJ_DIR)\d3dapp.res: d3dapp.rc resource.h
   $(rc) $(rcflags) $(OBJ_DIR)\d3dapp.res d3dapp.rc

$(OBJ_DIR)\$(proj).exe: $(OBJ_LIST) $(OBJ_DIR)\d3dapp.res
    $(link) $(linkdebug) $(linkflags) $(OBJ_LIST) $(borobjs) $(commas) $(outexe)$(OBJ_DIR)\$(proj).exe $(commas) $(commas) $(deflibs) $(d3dframe) ddraw.lib dxguid.lib $(commas) $(commas) $(OBJ_DIR)\d3dapp.res

mkdir:
    @if not exist $(OBJ_DIR)\NUL md $(OBJ_DIR)
    
# Rules for cleaning out those old files
clean:
    -@echo y | del Retail
    -@echo y | del Debug
    -@rd Retail
    -@rd Debug

proj=D3D_ScreenSaver

INCLUDE=$(INCLUDE);$(DXSDKROOT)\samples\multimedia\d3dim\include;

!if "$(nodebug)" == "1"
OBJ_DIR = Retail
!else
OBJ_DIR = Debug
!endif

!include "$(DXSDKROOT)\include\dxsdk.inc"

mycflags=

OBJ_LIST = $(OBJ_DIR)\render.obj $(OBJ_DIR)\screensaver.obj 

all: mkdir $(OBJ_DIR)\D3D.scr

$(OBJ_DIR)\render.obj:  render.cpp
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\render.obj render.cpp

$(OBJ_DIR)\screensaver.obj:  screensaver.cpp
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\screensaver.obj screensaver.cpp

$(OBJ_DIR)\screensaver.res: screensaver.rc resource.h
   $(rc) $(rcflags) $(OBJ_DIR)\screensaver.res screensaver.rc

$(OBJ_DIR)\d3d.scr: $(OBJ_LIST) $(OBJ_DIR)\screensaver.res
    $(link) $(linkdebug) $(linkflags) $(OBJ_LIST) $(borobjs) $(commas) $(outexe)$(OBJ_DIR)\D3D.scr $(commas) $(commas) $(deflibs) $(d3dframe) ddraw.lib dxguid.lib d3dxof.lib scrnsave.lib $(commas) $(commas) $(OBJ_DIR)\screensaver.res

mkdir:
    @if not exist $(OBJ_DIR)\NUL md $(OBJ_DIR)
    
# Rules for cleaning out those old files
clean:
    -@echo y | del Retail
    -@echo y | del Debug
    -@rd Retail
    -@rd Debug

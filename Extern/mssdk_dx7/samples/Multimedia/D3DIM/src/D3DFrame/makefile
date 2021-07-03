proj=d3dframe

INCLUDE=$(INCLUDE);$(DXSDKROOT)\samples\multimedia\d3dim\include;

!if "$(nodebug)" == "1"
OBJ_DIR = Retail
!else
OBJ_DIR = Debug
!endif

!include "$(DXSDKROOT)\include\dxsdk.inc"

mycflags=

OBJ_LIST = $(OBJ_DIR)\$(proj).obj $(OBJ_DIR)\d3denum.obj $(OBJ_DIR)\d3dmath.obj $(OBJ_DIR)\d3dtextr.obj $(OBJ_DIR)\d3dutil.obj $(OBJ_DIR)\d3dapp.obj $(OBJ_DIR)\d3dfile.obj

all: mkdir $(OBJ_DIR)\$(proj).lib copyfile

$(OBJ_DIR)\$(proj).obj:  $(proj).cpp
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\$(proj).obj $(proj).cpp

$(OBJ_DIR)\d3denum.obj:  d3denum.cpp
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\d3denum.obj d3denum.cpp

$(OBJ_DIR)\d3dfile.obj:  d3dfile.cpp
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\d3dfile.obj d3dfile.cpp

$(OBJ_DIR)\d3dmath.obj:  d3dmath.cpp
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\d3dmath.obj d3dmath.cpp

$(OBJ_DIR)\d3dtextr.obj:  d3dtextr.cpp
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\d3dtextr.obj d3dtextr.cpp

$(OBJ_DIR)\d3dutil.obj:  d3dutil.cpp
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\d3dutil.obj d3dutil.cpp

$(OBJ_DIR)\d3dapp.obj:  d3dapp.cpp
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\d3dapp.obj d3dapp.cpp

$(OBJ_DIR)\$(proj).lib: $(OBJ_LIST)
    $(bldlib) $(libflags) $(borlib)$(OBJ_DIR)\$(proj).obj $(borlib)$(OBJ_DIR)\d3denum.obj $(borlib)$(OBJ_DIR)\d3dmath.obj $(borlib)$(OBJ_DIR)\d3dtextr.obj $(borlib)$(OBJ_DIR)\d3dutil.obj $(borlib)$(OBJ_DIR)\d3dapp.obj $(borlib)$(OBJ_DIR)\d3dfile.obj

!IFDEF BORLAND
copyfile:
    copy /Y $(OBJ_DIR)\$(proj).lib $(OBJ_DIR)\..\..\..\lib\Borland

mkdir:
    @if not exist $(OBJ_DIR)\NUL md $(OBJ_DIR)
    @if not exist $(OBJ_DIR)\..\..\..\lib md $(OBJ_DIR)\..\..\..\lib
    @if not exist $(OBJ_DIR)\..\..\..\lib\Borland md $(OBJ_DIR)\..\..\..\lib\Borland
!ELSE
!IFDEF WATCOM
copyfile:
    copy /Y $(OBJ_DIR)\$(proj).lib $(OBJ_DIR)\..\..\..\lib\Watcom

mkdir:
    @if not exist $(OBJ_DIR)\NUL md $(OBJ_DIR)
    @if not exist $(OBJ_DIR)\..\..\..\lib md $(OBJ_DIR)\..\..\..\lib
    @if not exist $(OBJ_DIR)\..\..\..\lib\Watcom md $(OBJ_DIR)\..\..\..\lib\Watcom
!ELSE
copyfile:
    copy /Y $(OBJ_DIR)\$(proj).lib $(OBJ_DIR)\..\..\..\lib\

mkdir:
    @if not exist $(OBJ_DIR)\NUL md $(OBJ_DIR)
    @if not exist $(OBJ_DIR)\..\..\..\lib md $(OBJ_DIR)\..\..\..\lib
!ENDIF
!ENDIF

# Rules for cleaning out those old files
clean:
    -@echo y | del Retail
    -@echo y | del Debug
    -@rd Retail
    -@rd Debug

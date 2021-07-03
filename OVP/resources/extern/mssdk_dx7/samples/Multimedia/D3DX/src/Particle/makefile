proj=particle

INCLUDE=$(INCLUDE)

!if "$(nodebug)" == "1"
OBJ_DIR = Retail
!else
OBJ_DIR = Debug
!endif

!include "$(DXSDKROOT)\include\dxsdk.inc"

mycflags=

OBJ_LIST = $(OBJ_DIR)\$(proj).obj \
           $(OBJ_DIR)\cframetimer.obj \
           $(OBJ_DIR)\cparticle.obj \
           $(OBJ_DIR)\cground.obj
           
all: mkdir $(OBJ_DIR)\$(proj).exe

$(OBJ_DIR)\$(proj).obj:  $(proj).cpp cframetimer.hpp cparticle.hpp cground.hpp resource.h
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\$(proj).obj $(proj).cpp

$(OBJ_DIR)\cframetimer.obj:  cframetimer.cpp cframetimer.hpp 
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\cframetimer.obj cframetimer.cpp

$(OBJ_DIR)\cparticle.obj:  cparticle.cpp cparticle.hpp
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\cparticle.obj cparticle.cpp

$(OBJ_DIR)\cground.obj:  cground.cpp cground.hpp
    $(cc) $(cdebug) $(cflags) $(mycflags) $(outobj)$(OBJ_DIR)\cground.obj cground.cpp

$(OBJ_DIR)\$(proj).res: $(proj).rc resource.h
    $(rc) $(rcflags) $(OBJ_DIR)\$(proj).res $(proj).rc

$(OBJ_DIR)\$(proj).exe: $(OBJ_LIST) $(OBJ_DIR)\$(proj).res
    $(link) $(linkdebug) $(linkflags) $(OBJ_LIST) $(borobjs) $(commas) $(outexe)$(OBJ_DIR)\$(proj).exe $(commas) $(commas) $(deflibs) winmm.lib ddraw.lib d3dx.lib $(commas) $(commas) $(OBJ_DIR)\$(proj).res

mkdir:
    @if not exist $(OBJ_DIR)\NUL md $(OBJ_DIR)
    
# Rules for cleaning out those old files
clean:
    -@echo y | del Retail
    -@echo y | del Debug
    -@rd Retail
    -@rd Debug

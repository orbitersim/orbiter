NAME = duel
EXT = exe
GLOBAL_RECOMPILE = $(DXROOT)\dplay\dplay\recompdp.log
#WANTASM=1
IS_32 = 1

GOALS = $(PBIN)\$(NAME).$(EXT)

LIBS    =libcmt.lib kernel32.lib user32.lib advapi32.lib ddraw.lib dplayx.lib \
         comdlg32.lib gdi32.lib winmm.lib libc.lib comctl32.lib \
         dinput.lib dsound.lib ole32.lib

OBJS    =  duel.obj ddutil.obj util.obj wizard.obj gameproc.obj gfx.obj comm.obj \
           input.obj lobby.obj ds3dutil.obj sfx.obj
	  
!if "$(DEBUG)" == "debug"
COPT =-YX -DSTRICT -DDEBUG -Zi -Fd$(NAME).PDB -I$(DXROOT)\samples\inc
AOPT =-DDEBUG
LOPT =-debug:full -debugtype:cv -pdb:$(NAME).pdb
ROPT =-DDEBUG
!else
COPT =-YX -DSTRICT 
AOPT =
LOPT =-debug:none
ROPT =
!endif
RES = $(NAME).res 

!if ("$(DEBUG)" == "ntretail") || ("$(DEBUG)" == "ntdebug")
CFLAGS	=$(COPT) -Oxa -D_X86_ $(CDEBUG) -Fo$@
!else
CFLAGS  =$(COPT) -Oxa -D_X86_ $(CDEBUG) -Fo$@ 
!endif
AFLAGS	=$(AOPT) -Zp4 -DSTD_CALL -DBLD_COFF -coff
LFLAGS  =$(LOPT)
RCFLAGS	=$(ROPT) -i $(DEVROOT)\msdev\mfc\include

!include ..\..\..\proj.mk

# Add in the sample lib path
LIB = $(LIB);$(DXROOT)\samples\lib

$(NAME).$(EXT): \
	$(OBJS) ..\default.mk $(RES)
	@$(LINK) $(LFLAGS) @<<
-out:$(NAME).$(EXT)
-map:$(NAME).map
-machine:i386
-subsystem:windows,4.0
$(LIBS)
$(RES)
$(OBJS)
<<

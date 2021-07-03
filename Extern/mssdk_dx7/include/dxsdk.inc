!IFDEF BORLAND
cc = bcc32
rc = brc32
link = ilink32
bldlib = tlib
cflags = -c -3 -j1 -X -Vd -I$(INCLUDE)
cflags = $(cflags) -Dsinf=(FLOAT)sin
cflags = $(cflags) -Dcosf=(FLOAT)cos
cflags = $(cflags) -Dasinf=(FLOAT)asin
cflags = $(cflags) -Dacosf=(FLOAT)acos
cflags = $(cflags) -Dsqrtf=(FLOAT)sqrt
cflags = $(cflags) -Dexpf=(FLOAT)exp
rcflags = -r -fo
linkflags = -n -V4.0 -aa 
!IFDEF main
linkflags = $(linkflags) -ap
borobjs=c0x32.obj
!ELSE
linkflags = $(linkflags) -Tpe
borobjs=c0w32.obj
!ENDIF
linkflags = $(linkflags) -j$(LIB) -L$(LIB)
libflags=$(OBJ_DIR)\$(proj).lib
outobj = -o
commas =,
outexe=
borlib=+
deflibs = import32.lib cw32mti.lib ole2w32.lib
!IFDEF MFC
cflags =$(cflags) -VF -w-hid -w-par -w-aus -w-inl -w-inq -WM -Vd /D_NO_VCL;_RTLDLL;_AFXDLL;_X86_;_WINDOWS;_MSC_VER=1100;WIN32;
!IFDEF NODEBUG
deflibs = bfc42.lib bfcs42.lib $(deflibs)
!ELSE
deflibs = BFC42D.lib BFCS42D.lib bfcd42d.lib bfco42d.lib bfcn42d.lib $(deflibs)
!ENDIF
!ENDIF

d3dframe=..\..\lib\borland\d3dframe.lib

!IFDEF nodebug
cdebug = -Od
linkdebug = /x
cflags = $(cflags) /DNDEBUG;
!ELSE
cdebug = -Od -v -y
linkdebug = /v
cflags = $(cflags) /DDEBUG;
!ENDIF

!ELSE
!IFDEF WATCOM
cc = cl
rc = rc
link = link
bldlib = lib
deflibs = kernel32.lib gdi32.lib user32.lib advapi32.lib gdi32.lib comdlg32.lib winspool.lib shell32.lib winmm.lib comctl32.lib ole32.lib oleaut32.lib
cflags = -c /D "WIN32" /D "_WINDOWS"
cflags = $(cflags) -Dsinf=sin
cflags = $(cflags) -Dcosf=cos
cflags = $(cflags) -Dasinf=asin
cflags = $(cflags) -Dacosf=acos
cflags = $(cflags) -Dsqrtf=sqrt
cflags = $(cflags) -Dexpf=exp
!IFDEF NODEBUG
cflags =$(cflags) /D "NDEBUG"
cdebug = -Ox
linkdebug =
!IFDEF MFC
cflags =$(cflags) /MD /D "_AFXDLL"
!ELSE
cflags =$(cflags) /ML
!ENDIF
!ELSE
cflags =$(cflags) /D "DEBUG"
cdebug = -Z7 -Ox
linkdebug = -map -debug:full
!IFDEF MFC
cflags =$(cflags) /MDd /D "_AFXDLL"
!ELSE
cflags =$(cflags) /MLd
!ENDIF
!ENDIF
rcflags = -r -fo
linkflags = /INCREMENTAL:NO /NOLOGO
!IFDEF main
linkflags = $(linkflags) /SUBSYSTEM:CONSOLE
!ELSE
linkflags = $(linkflags) /SUBSYSTEM:WINDOWS
!ENDIF
libflags=/OUT:$(OBJ_DIR)\$(proj).lib
outobj = -Fo
commas=
outexe=-out:
borobjs=
borlib=
d3dframe=..\..\lib\watcom\d3dframe.lib

!ELSE
cc = cl
rc = rc
link = link
bldlib = lib
deflibs = kernel32.lib gdi32.lib user32.lib advapi32.lib gdi32.lib comdlg32.lib winspool.lib shell32.lib winmm.lib comctl32.lib ole32.lib oleaut32.lib
cflags = -c /D "WIN32" /D "_WINDOWS"
linkflags=
!IFDEF NODEBUG
cflags =$(cflags) /D "NDEBUG"
cdebug = -Ox
linkdebug = /RELEASE
!IFDEF MFC
cflags =$(cflags) /MD /D "_AFXDLL"
!ELSE
cflags =$(cflags) /ML
!ENDIF
!ELSE
cflags = $(cflags) /D "DEBUG"
cdebug = -Z7 -Ox
linkdebug = -map -debug:full -debugtype:cv
!IFDEF MFC
cflags =$(cflags) /MDd /D "_AFXDLL"
!ELSE
linkflags = $(linkflags) /NODEFAULTLIB:LIBC
cflags =$(cflags) /MLd
!ENDIF
!ENDIF
cflags =$(cflags) 
rcflags = -r -fo
linkflags = $(linkflags) /INCREMENTAL:NO /PDB:NONE /NOLOGO
!IFDEF main
linkflags = $(linkflags) /SUBSYSTEM:CONSOLE
!ELSE
linkflags = $(linkflags) /SUBSYSTEM:WINDOWS
!ENDIF
libflags=/OUT:$(OBJ_DIR)\$(proj).lib /SUBSYSTEM:WINDOWS
outobj = -Fo
commas=
outexe=-out:
borobjs=
borlib=
d3dframe=..\..\lib\d3dframe.lib

!ENDIF
!ENDIF

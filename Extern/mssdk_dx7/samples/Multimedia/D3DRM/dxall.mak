SAMPLES = \
	src\convxfmt \
	src\Faces \
	src\Fly \
        src\FlyFS \
	src\Globe \
	src\Hier1 \
	src\Hier2 \
	src\Loadfile \
	src\Morph \
	src\Quat \
	src\rmbegin1 \
	src\rmbegin2 \
	src\rmenum \
	src\rmshadow \
	src\Tex1 \
	src\Tex3 \
	src\Tex4 \
	src\Tex5 \
	src\Trans \
	src\Uvis \
	src\Viewer \
	src\xofload \
	src\xofsave \
	src\xofuser

!IFDEF clean
do=-i clean
DOING=Clean
!ELSE

!IFDEF nodebug
do=nodebug=1
DOING=Release
!ELSE
DOING=Debug
!ENDIF
!ENDIF

$(SAMPLES): $(@R)\makefile
        @cd $@
        @echo *** DirectX\$@ $(DOING)***
        @echo *** DirectX\$@ $(DOING)*** >>..\DirectX.tmp
        @nmake $(do) >>..\DirectX.tmp
        @cd ..\..

SAMPLES = \
	src\DDEnum \
	src\DDex1 \
	src\DDex2 \
	src\DDex3 \
	src\DDex4 \
	src\DDex5 \
	src\DDoverlay \
	src\Donuts \
	src\Dxtex \
	src\Font \
	src\FSWindow \
	src\ModeTest \
	src\Mosquito \
	src\Multimon \
	src\Stretch \
	src\Switcher \
	src\Wormhole

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

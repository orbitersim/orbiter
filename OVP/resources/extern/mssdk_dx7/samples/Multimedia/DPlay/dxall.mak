SAMPLES = \
	src\BellHop       \
	src\ChatConnect   \
	src\DPLaunch      \
	src\DPSlots       \
	src\Duel          \
	src\SimpleConnect \
	src\StagedConnect \
	src\Override

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

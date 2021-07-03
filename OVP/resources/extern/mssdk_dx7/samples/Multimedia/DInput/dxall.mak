SAMPLES = \
        src\Tutorials\DIEx1 \
        src\Tutorials\DIEx2 \
        src\Tutorials\DIEx3 \
        src\Tutorials\DIEx4 \
        src\Tutorials\DIEx5 \
        src\DIGame   \
        src\FFDonuts \
        src\FFFileRead \
        src\Scrawl   \
        src\MouseNon \
        src\MouseExc \
        src\KeybdImm \
        src\KeybdExc \
        src\KeybdBuf \
        src\JoyFFeed \
        src\JoystImm


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
        @cd $(DXSDKROOT)\samples\multimedia\dinput


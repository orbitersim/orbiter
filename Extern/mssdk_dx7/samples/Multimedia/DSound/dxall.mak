SAMPLES = \
        src\AdjustSound      \
        src\CaptureSound     \
        src\EnumDevices      \
        src\FullDuplexFilter \
        src\Play3DSound      \
        src\PlaySound        \
        src\PlayStreaming    \
        src\VoiceManagement

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

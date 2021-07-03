SAMPLES = \
	src\dmboids \
	src\dmdonuts \
	src\dmhook \
        src\dmusmidi \
        src\3dmusic \
	src\dmshell \
	src\echotool \
	src\musiclines \
	src\playmotf \
	src\playpri

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

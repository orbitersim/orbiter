SAMPLES = \
        src\Tutorials\DrawPrims \
        src\Tutorials\Enumerate \
        src\Tutorials\Texture \
        src\Tutorials\Triangle \
        src\Tutorials\ZBuffer \
	src\Bend \
	src\Billboard \
	src\Boids \
        src\BumpEarth \
        src\BumpWaves \
        src\ClipMirror \
        src\Compress \
        src\Dolphin \
        src\EnumTex \
        src\Envcube \
	src\Filter \
	src\Fireworks \
	src\Flare \
        src\LightMap \
	src\Lights \
        src\MFCFog \
        src\MFCTex \
	src\MipMap \
	src\MTexture \
        src\ScreenSaver \
        src\ShadowVol \
        src\ShadowVol2 \
	src\SphereMap \
        src\StencilDepth \
        src\StencilMirror \
        src\VBuffer \
        src\VideoTex \
        src\WBuffer \
        src\XFile \
	src\D3dFrame

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
        @cd $(DXSDKROOT)\samples\multimedia\d3dim

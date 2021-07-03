set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR})

# Add the executable
add_executable(Orbiter
	Astro.cpp
	Camera.cpp
	Config.cpp
	ddeserver.cpp
	Element.cpp
	elevmgr.cpp
	Help.cpp
	Input.cpp
	Keymap.cpp
	LightEmitter.cpp
	Mesh.cpp
	Nav.cpp
	Orbiter.cpp
	PlaybackEd.cpp
	Psys.cpp
	Script.cpp
	Shadow.cpp
	State.cpp
	Vecmat.cpp
	VectorMap.cpp
	# API
	CamAPI.cpp
	DrawAPI.cpp
	GraphicsAPI.cpp
	MFDAPI.cpp
	ModuleAPI.cpp
	OrbiterAPI.cpp
	# Body
	Body.cpp
	BodyIntegrator.cpp
	Celbody.cpp
	Planet.cpp
	Rigidbody.cpp
	Star.cpp
		# Vessel
		FlightRecorder.cpp
		SuperVessel.cpp
		Vessel.cpp
		Vesselbase.cpp
		Vesselstatus.cpp
		# Base
		Base.cpp
		Baseobj.cpp
	# Cockpit
	Defpanel.cpp
	hud.cpp
	MenuInfoBar.cpp
	Pane.cpp
	Panel.cpp
	Panel2D.cpp
	VCockpit.cpp
	# Dialogs
	CustomControls.cpp
	Dialogs.cpp
	DialogWin.cpp
	DlgCamera.cpp
	DlgCapture.cpp
	DlgFocus.cpp
	DlgFunction.cpp
	DlgHelp.cpp
	DlgInfo.cpp
	DlgMap.cpp
	DlgMenuCfg.cpp
	DlgMgr.cpp
	DlgRecorder.cpp
	DlgTacc.cpp
	DlgVishelper.cpp
	htmlctrl.c
	Select.cpp
	# DirectX7 interface
	D3d7util.cpp
	D3dmath.cpp
	Di7frame.cpp
	# Graphics
	cloudmgr2.cpp
	CSphereMgr.cpp
	cspheremgr2.cpp
	D3d7enum.cpp
	D3d7frame.cpp
	ExtraRender.cpp
	${GDICLIENT_DIR}/GDIClient.cpp
	OGraphics.cpp
	Particle.cpp
	Scene.cpp
	ScreenNote.cpp
	Spherepatch.cpp
	surfmgr2.cpp
	Texture.cpp
	tilelabel.cpp
	TileMgr.cpp
	tilemgr2.cpp
	VBase.cpp
	Vobject.cpp
	VPlanet.cpp
	Vstar.cpp
	Vvessel.cpp
	# Launchpad
	Launchpad.cpp
	LpadTab.cpp
	TabAbout.cpp
	TabExtra.cpp
	TabJoystick.cpp
	TabModule.cpp
	TabParam.cpp
	TabScenario.cpp
	TabVideo.cpp
	TabVisual.cpp
	# MFD
	Mfd.cpp
	MfdAlign.cpp
	MfdComms.cpp
	MfdDocking.cpp
	MfdHsi.cpp
	MfdLanding.cpp
	MfdMap.cpp
	MfdMap_old.cpp
	MfdOrbit.cpp
	MfdSurface.cpp
	MfdSync.cpp
	MfdTransfer.cpp
	MfdUser.cpp
	# Utils
	Log.cpp
	Memstat.cpp
	Util.cpp
	ZTreeMgr.cpp
	# Resources
	Orbiter.rc
	Orbiter.rc2
	Orbiter.ico
	# Files built on the fly
	cryptstring.h
	Disclaimer.txt
	splash_cs.dat
	)

add_definitions(-DINLINEGRAPHICS)

target_include_directories(Orbiter
	PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}
	PUBLIC ${CMAKE_SOURCE_DIR}
	PUBLIC ${CMAKE_SOURCE_DIR}/Orbitersdk/include
	PUBLIC ${CMAKE_SOURCE_DIR}/OVP
	PUBLIC ${CMAKE_CURRENT_BINARY_DIR}
	PUBLIC ${ZLIB_INCLUDE_DIR}
	PUBLIC ${DX7SDK_INCLUDE_DIR})

target_link_libraries(Orbiter
	Winmm.dll
	comctl32.lib
	UxTheme.lib
	${DX7SDK_LIB_DIR}/dxguid.lib
	${DX7SDK_LIB_DIR}/d3dim.lib
	${DX7SDK_LIB_DIR}/ddraw.lib
	${DX7SDK_LIB_DIR}/dinput.lib
	${HTML_HELP_LIBRARY}
	${ZLIB_LIBRARIES}
	${CMAKE_BINARY_DIR}/Core/Orbitersdk/Orbitersdk.lib
	${CMAKE_BINARY_DIR}/Core/DlgCtrl/DlgCtrl.lib
	)

add_dependencies(Orbiter
	scramble
	fchecksum
	Orbitersdk
	DlgCtrl
	)

set_target_properties(Orbiter PROPERTIES LINK_FLAGS "/SUBSYSTEM:WINDOWS")

add_custom_command(
	OUTPUT cryptstring.h
	COMMAND ${CMAKE_BINARY_DIR}/Utils/scramble/scramble.exe ${CMAKE_SOURCE_DIR}/cryptstring.h.in cryptstring.h
)

add_custom_command(
	OUTPUT Disclaimer.txt
	COMMAND ${CMAKE_BINARY_DIR}/Utils/scramble/scramble.exe ${CMAKE_SOURCE_DIR}/Disclaimer.txt.in Disclaimer.txt
)

add_custom_command(
	OUTPUT splash_cs.dat
	COMMAND ${CMAKE_BINARY_DIR}/Utils/fchecksum/fchecksum.exe ${CMAKE_SOURCE_DIR}/Bitmaps/Splash2016.jpg splash_cs.dat
)

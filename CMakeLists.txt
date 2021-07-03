cmake_minimum_required(VERSION 3.10)

# Set the project name
project (Orbiter VERSION 2021.1)

set(OVP_DIR "${CMAKE_SOURCE_DIR}/OVP")
set(GDICLIENT_DIR "${OVP_DIR}/GDIClient")

set(EXTERN_PATH "${CMAKE_SOURCE_DIR}/Extern")
set(DX7SDK_PATH "${EXTERN_PATH}/mssdk_dx7")
set(DX7SDK_INCLUDE_DIR "${DX7SDK_PATH}/include")
set(DX7SDK_LIB_DIR "${DX7SDK_PATH}/lib")

#set(CMAKE_PREFIX_PATH "${CMAKE_SOURCE_DIR}/../extern")

find_package(ZLIB)
find_package(HTMLHelp)

if(MSVC)
  set(CMAKE_EXE_LINKER_FLAGS
      "${CMAKE_EXE_LINKER_FLAGS} /NODEFAULTLIB:\"LIBCMTD\"")
endif()

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
		Supervessel.cpp
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
	VObject.cpp
	VPlanet.cpp
	VStar.cpp
	VVessel.cpp
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
	)

add_definitions(-DINLINEGRAPHICS)

target_include_directories(Orbiter
	PUBLIC ${CMAKE_SOURCE_DIR}
	PUBLIC ${CMAKE_SOURCE_DIR}/Orbitersdk/include
	PUBLIC ${CMAKE_SOURCE_DIR}/OVP
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

# Sub-projects
add_subdirectory(Core)
add_subdirectory(Utils)

add_dependencies(Orbiter
	Orbitersdk
	DlgCtrl
	)

set_target_properties(Orbiter PROPERTIES LINK_FLAGS_DEBUG "/SUBSYSTEM:WINDOWS")
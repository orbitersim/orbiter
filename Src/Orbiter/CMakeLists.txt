# Copyright (c) Martin Schweiger
# Licensed under the MIT License

# SAFESEH linker flag must be turned off because the DX7 libraries don't support it'
set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -SAFESEH:NO")

# Sources for all Orbiter executable targets
set(common_src
# General source files
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
# Body classes
	Body.cpp
	BodyIntegrator.cpp
	Celbody.cpp
	Planet.cpp
	Rigidbody.cpp
	Star.cpp
# Vessel classes
	FlightRecorder.cpp
	SuperVessel.cpp
	Vessel.cpp
	Vesselbase.cpp
	Vesselstatus.cpp
# Surface base classes
	Base.cpp
	Baseobj.cpp
# Cockpit classes
	Defpanel.cpp
	hud.cpp
	MenuInfoBar.cpp
	Pane.cpp
	Panel.cpp
	Panel2D.cpp
	VCockpit.cpp
# MFD classes
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
# API implementations
	CamAPI.cpp
	DrawAPI.cpp
	GraphicsAPI.cpp
	MFDAPI.cpp
	ModuleAPI.cpp
	OrbiterAPI.cpp
# Graphics utils
	D3d7util.cpp
	D3dmath.cpp
	Di7frame.cpp
# Graphics interface base class for GDI clients
	${GDICLIENT_DIR}/GDIClient.cpp
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

# Sources for DX7 built-in graphics version only
set(graphics_src
# DX7 inline graphics
	cloudmgr2.cpp
	CSphereMgr.cpp
	cspheremgr2.cpp
	D3d7enum.cpp
	D3d7frame.cpp
	ExtraRender.cpp
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
)


set(Orbiter_sources)

# General source files
list(APPEND Orbiter_sources
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
)

# Launchpad
list(APPEND Orbiter_sources
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
)

# Dialogs
list(APPEND Orbiter_sources
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
)

# Body classes
list(APPEND Orbiter_sources
	Body.cpp
	BodyIntegrator.cpp
	Celbody.cpp
	Planet.cpp
	Rigidbody.cpp
	Star.cpp
)

# Vessel classes
list(APPEND Orbiter_sources
	FlightRecorder.cpp
	SuperVessel.cpp
	Vessel.cpp
	Vesselbase.cpp
	Vesselstatus.cpp
)

# Surface base classes
list(APPEND Orbiter_sources
	Base.cpp
	Baseobj.cpp
)

# Cockpit classes
list(APPEND Orbiter_sources
	Defpanel.cpp
	hud.cpp
	MenuInfoBar.cpp
	Pane.cpp
	Panel.cpp
	Panel2D.cpp
	VCockpit.cpp
)

# MFD modes
list(APPEND Orbiter_sources
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
)

# API implementations
list(APPEND Orbiter_sources
	CamAPI.cpp
	DrawAPI.cpp
	GraphicsAPI.cpp
	MFDAPI.cpp
	ModuleAPI.cpp
	OrbiterAPI.cpp
)

# Graphics utils
list(APPEND Orbiter_sources
	D3d7util.cpp
	D3dmath.cpp
	Di7frame.cpp
)

# Graphics interface base class for GDI clients
list(APPEND Orbiter_sources
	${GDICLIENT_DIR}/GDIClient.cpp
)

# DX7 inline graphics
if(ORBITER_BUILD_WITH_DX7)
list(APPEND Orbiter_sources
	cloudmgr2.cpp
	CSphereMgr.cpp
	cspheremgr2.cpp
	D3d7enum.cpp
	D3d7frame.cpp
	ExtraRender.cpp
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
)
endif()

# Utils
list(APPEND Orbiter_sources
	Log.cpp
	Memstat.cpp
	Util.cpp
	ZTreeMgr.cpp
)

# Resources
list(APPEND Orbiter_sources
	Orbiter.rc
	Orbiter.rc2
	Orbiter.ico
)

# Files built on the fly
list(APPEND Orbiter_sources
	cryptstring.h
	Disclaimer.txt
	splash_cs.dat
)

# Orbiter executable (DX7 inline graphics version)
add_executable(Orbiter
	${common_src}
	${graphics_src}
)

# Orbiter executable (graphics server version)
add_executable(Orbiter_server
	${common_src}
)

#if(ORBITER_BUILD_WITH_DX7)
#	add_definitions(-DINLINEGRAPHICS)
set_target_properties(Orbiter
	PROPERTIES
	LINK_FLAGS "/SUBSYSTEM:WINDOWS"
	RUNTIME_OUTPUT_DIRECTORY ${ORBITER_BINARY_ROOT_DIR}
	COMPILE_DEFINITIONS INLINEGRAPHICS
	ENABLE_EXPORTS 1
	FOLDER Core
)
#else()
#	add_definitions(-DNOGRAPHICS)
set_target_properties(Orbiter_server
	PROPERTIES
	LINK_FLAGS "/SUBSYSTEM:WINDOWS"
	RUNTIME_OUTPUT_DIRECTORY ${ORBITER_BINARY_MODULE_DIR}/Server
	RUNTIME_OUTPUT_NAME Orbiter
	COMPILE_DEFINITIONS NOGRAPHICS
	ENABLE_EXPORTS 1
	FOLDER Core
)
#endif()

set(orbiter_includes
	${CMAKE_CURRENT_SOURCE_DIR}
	${CMAKE_SOURCE_DIR}/Orbitersdk/include
	${CMAKE_SOURCE_DIR}/OVP
	${CMAKE_CURRENT_BINARY_DIR}
	${ZLIB_INCLUDE_DIR}
	${DX7SDK_INCLUDE_DIR}
)
target_include_directories(Orbiter PUBLIC
	${orbiter_includes}
)
target_include_directories(Orbiter_server PUBLIC
	${orbiter_includes}
)

#target_include_directories(Orbiter
#	PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}
#	PUBLIC ${CMAKE_SOURCE_DIR}/Orbitersdk/include
#	PUBLIC ${CMAKE_SOURCE_DIR}/OVP
#	PUBLIC ${CMAKE_CURRENT_BINARY_DIR}
#	PUBLIC ${ZLIB_INCLUDE_DIR}
#	PUBLIC ${DX7SDK_INCLUDE_DIR}
#)

set(orbiter_libs
	Winmm.dll
	comctl32.lib
	UxTheme.lib
	${DX7SDK_LIB_DIR}/dxguid.lib
	${DX7SDK_LIB_DIR}/d3dim.lib
	${DX7SDK_LIB_DIR}/ddraw.lib
	${DX7SDK_LIB_DIR}/dinput.lib
	${HTML_HELP_LIBRARY}
	${ZLIB_LIBRARIES}
	$<TARGET_FILE:Orbitersdk>
	$<TARGET_FILE:DlgCtrl>
)
target_link_libraries(Orbiter
	${orbiter_libs}
)
target_link_libraries(Orbiter_server
	${orbiter_libs}
)

#target_link_libraries(Orbiter
#	Winmm.dll
#	comctl32.lib
#	UxTheme.lib
#	${DX7SDK_LIB_DIR}/dxguid.lib
#	${DX7SDK_LIB_DIR}/d3dim.lib
#	${DX7SDK_LIB_DIR}/ddraw.lib
#	${DX7SDK_LIB_DIR}/dinput.lib
#	${HTML_HELP_LIBRARY}
#	${ZLIB_LIBRARIES}
#	$<TARGET_FILE:Orbitersdk>
#	$<TARGET_FILE:DlgCtrl>
#)

set(orbiter_depends
	scramble
	fchecksum
	Orbitersdk
	DlgCtrl
)
add_dependencies(Orbiter
	${orbiter_depends}
)
add_dependencies(Orbiter_server
	${orbiter_depends}
)


#add_dependencies(Orbiter
#	scramble
#	fchecksum
#	Orbitersdk
#	DlgCtrl
#	)

#set_target_properties(Orbiter
#	PROPERTIES
#	LINK_FLAGS "/SUBSYSTEM:WINDOWS"
#	ENABLE_EXPORTS 1
#	FOLDER Core
#)

add_custom_command(
	OUTPUT cryptstring.h
	DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/cryptstring.h.in
	COMMAND $<TARGET_FILE:scramble> ${CMAKE_CURRENT_SOURCE_DIR}/cryptstring.h.in cryptstring.h
)

add_custom_command(
	OUTPUT Disclaimer.txt
	DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/Disclaimer.txt.in
	COMMAND $<TARGET_FILE:scramble> ${CMAKE_CURRENT_SOURCE_DIR}/Disclaimer.txt.in Disclaimer.txt
)

add_custom_command(
	OUTPUT splash_cs.dat
	DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/Bitmaps/Splash.jpg
	COMMAND $<TARGET_FILE:fchecksum> ${CMAKE_CURRENT_SOURCE_DIR}/Bitmaps/Splash.jpg splash_cs.dat
)


# Installation: Orbiter executable (standalone or graphics-server version)
#if(ORBITER_BUILD_WITH_DX7)
	install(TARGETS
		Orbiter
		RUNTIME
		DESTINATION ${ORBITER_INSTALL_ROOT_DIR}
	)
#else()
	install(TARGETS
		Orbiter_server
		RUNTIME
		DESTINATION ${ORBITER_INSTALL_MODULE_DIR}/Server
	)
#endif()

# Installation: Orbiter SDK interface library to SDK folder to be included by addons
install(FILES $<TARGET_LINKER_FILE:Orbiter>
	DESTINATION ${ORBITER_INSTALL_SDK_DIR}/lib
)
# Copyright (c) Martin Schweiger
# Licensed under the MIT License

FetchContent_MakeAvailable(imgui)
FetchContent_MakeAvailable(implot)

# Sources for all Orbiter executable targets
set(common_src
# General source files
	Astro.cpp
	Camera.cpp
	cmdline.cpp
	Config.cpp
	console_ng.cpp
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
	TimeData.cpp
# Launchpad
	Launchpad.cpp
	LaunchpadRenderer.mm
	TabAbout.cpp
	TabExtra.cpp
	TabModule.cpp
	TabOptions.cpp
	TabScenario.cpp
	TabVideo.cpp
# Dialogs
	DlgCamera.cpp
	DlgCapture.cpp
	DlgFocus.cpp
	DlgFunction.cpp
	DlgHelp.cpp
	DlgInfo.cpp
	DlgMap.cpp
	DlgMenuCfg.cpp
	DlgMgr.cpp
	DlgOptions.cpp
	DlgRecorder.cpp
	DlgTacc.cpp
	Select.cpp
# Body classes
	Body.cpp
	BodyIntegrator.cpp
	PinesGrav.cpp
	Celbody.cpp
	Planet.cpp
	Rigidbody.cpp
	Star.cpp
# Vessel classes
	FlightRecorder.cpp
	CollisionCore.cpp
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
	CelSphereAPI.cpp
	DrawAPI.cpp
	GraphicsAPI.cpp
	MFDAPI.cpp
	ModuleAPI.cpp
	OrbiterAPI.cpp
# Graphics utils
	D3dmath.cpp
# Utils
	Log.cpp
	Memstat.cpp
	Util.cpp
	ZTreeMgr.cpp
# ImGui platform and renderer backends
	imgui_impl_orbiter.cpp
	${imgui_SOURCE_DIR}/backends/imgui_impl_metal.mm
)

set(Orbiter_includes
	${CMAKE_CURRENT_SOURCE_DIR}
	${CMAKE_SOURCE_DIR}/Orbitersdk/include
	${CMAKE_SOURCE_DIR}/OVP
	${CMAKE_CURRENT_BINARY_DIR}
)

set(Orbiter_common_libs
	zlib
	Orbitersdk
	OrbiterPlatform
)
set(Orbiter_libs
)

set(Orbiter_depends
	Orbitersdk
)

add_library(OrbiterCore SHARED ${common_src})

set_target_properties(OrbiterCore
	PROPERTIES
	LIBRARY_OUTPUT_DIRECTORY ${ORBITER_BINARY_ROOT_DIR}
	INSTALL_NAME_DIR "@rpath"
	FOLDER Core
)

target_include_directories(OrbiterCore PUBLIC ${Orbiter_includes} ${TRACY_CLIENT_INCLUDE} ${imgui_SOURCE_DIR}/ ${imgui_SOURCE_DIR}/backends ${implot_SOURCE_DIR}/)
target_link_libraries(OrbiterCore ${Orbiter_common_libs} ${Orbiter_libs} ${TRACY_CLIENT})
target_compile_options(OrbiterCore PRIVATE "$<$<COMPILE_LANGUAGE:OBJCXX>:-fobjc-arc>")
set_property(SOURCE ${imgui_SOURCE_DIR}/backends/imgui_impl_metal.mm PROPERTY COMPILE_OPTIONS "-fobjc-arc")
add_dependencies(OrbiterCore ${Orbiter_depends})

add_executable(Orbiter Main.cpp)
set_target_properties(Orbiter
	PROPERTIES
	RUNTIME_OUTPUT_DIRECTORY ${ORBITER_BINARY_ROOT_DIR}
	OUTPUT_NAME Orbiter
	BUILD_RPATH "@executable_path"
	INSTALL_RPATH "@executable_path"
	FOLDER Core
)
target_link_libraries(Orbiter OrbiterCore)

install(TARGETS Orbiter RUNTIME DESTINATION ${ORBITER_INSTALL_ROOT_DIR})
install(TARGETS OrbiterCore LIBRARY DESTINATION ${ORBITER_INSTALL_ROOT_DIR})

string(TIMESTAMP DATE "%d %b %Y")
string(TIMESTAMP VERSION "%y%m%d")
configure_file(about.hpp.in about.hpp @ONLY)

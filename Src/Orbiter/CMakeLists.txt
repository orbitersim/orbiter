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

set(ORBITER_ICONSET ${CMAKE_CURRENT_BINARY_DIR}/Orbiter.iconset)
set(ORBITER_ICNS ${CMAKE_CURRENT_BINARY_DIR}/Orbiter.icns)
add_custom_command(OUTPUT ${ORBITER_ICNS}
	COMMAND ${CMAKE_COMMAND} -E make_directory ${ORBITER_ICONSET}
	COMMAND sips -s format png ${CMAKE_CURRENT_SOURCE_DIR}/Orbiter.ico --out ${ORBITER_ICONSET}/icon_256x256.png > /dev/null
	COMMAND sips -z 16 16 ${ORBITER_ICONSET}/icon_256x256.png --out ${ORBITER_ICONSET}/icon_16x16.png > /dev/null
	COMMAND sips -z 32 32 ${ORBITER_ICONSET}/icon_256x256.png --out ${ORBITER_ICONSET}/icon_16x16@2x.png > /dev/null
	COMMAND sips -z 32 32 ${ORBITER_ICONSET}/icon_256x256.png --out ${ORBITER_ICONSET}/icon_32x32.png > /dev/null
	COMMAND sips -z 64 64 ${ORBITER_ICONSET}/icon_256x256.png --out ${ORBITER_ICONSET}/icon_32x32@2x.png > /dev/null
	COMMAND sips -z 128 128 ${ORBITER_ICONSET}/icon_256x256.png --out ${ORBITER_ICONSET}/icon_128x128.png > /dev/null
	COMMAND ${CMAKE_COMMAND} -E copy ${ORBITER_ICONSET}/icon_256x256.png ${ORBITER_ICONSET}/icon_128x128@2x.png
	COMMAND iconutil -c icns ${ORBITER_ICONSET} -o ${ORBITER_ICNS}
	DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/Orbiter.ico
)
set_source_files_properties(${ORBITER_ICNS} PROPERTIES MACOSX_PACKAGE_LOCATION Resources)

string(TIMESTAMP ORBITER_BUNDLE_VERSION "%y.%m.%d")
configure_file(${CMAKE_SOURCE_DIR}/cmake/Info.plist.in ${CMAKE_CURRENT_BINARY_DIR}/Info.plist @ONLY)

add_executable(Orbiter MACOSX_BUNDLE Main.cpp ${ORBITER_ICNS})
set_target_properties(Orbiter
	PROPERTIES
	RUNTIME_OUTPUT_DIRECTORY ${ORBITER_BINARY_ROOT_DIR}
	OUTPUT_NAME Orbiter
	MACOSX_BUNDLE_INFO_PLIST ${CMAKE_CURRENT_BINARY_DIR}/Info.plist
	BUILD_RPATH "@executable_path/../../.."
	INSTALL_RPATH "@executable_path/../../.."
	FOLDER Core
)
target_link_libraries(Orbiter OrbiterCore)
add_custom_command(TARGET Orbiter POST_BUILD
	COMMAND ${CMAKE_COMMAND} -E create_symlink Orbiter.app/Contents/MacOS/Orbiter ${ORBITER_BINARY_ROOT_DIR}/Orbiter
	COMMAND codesign --force --sign - ${ORBITER_BINARY_ROOT_DIR}/Orbiter.app > /dev/null 2>&1 || true
)

install(TARGETS Orbiter BUNDLE DESTINATION ${ORBITER_INSTALL_ROOT_DIR})
install(TARGETS OrbiterCore LIBRARY DESTINATION ${ORBITER_INSTALL_ROOT_DIR})

string(TIMESTAMP DATE "%d %b %Y")
string(TIMESTAMP VERSION "%y%m%d")
configure_file(about.hpp.in about.hpp @ONLY)

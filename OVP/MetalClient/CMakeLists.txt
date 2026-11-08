# Copyright (c) Martin Schweiger
# Licensed under the MIT License

project(MetalClient VERSION 31.0)

set(D3D9_VERSION_MAJOR ${PROJECT_VERSION_MAJOR})
set(D3D9_VERSION_MINOR ${PROJECT_VERSION_MINOR})
configure_file(D3D9ClientConfig.h.in D3D9ClientConfig.h)
FetchContent_MakeAvailable(imgui)

set(CLIENT_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR})
set(INCLUDE_TARGET_DIR ${ORBITER_BINARY_SDK_DIR}/include)

find_package(Python3 REQUIRED COMPONENTS Interpreter)

add_custom_command(OUTPUT ${INCLUDE_TARGET_DIR}/gcCoreAPI.h ${INCLUDE_TARGET_DIR}/gcGUI.h
	DEPENDS ${CLIENT_SOURCE_DIR}/gcGUI.h ${CLIENT_SOURCE_DIR}/gcCore.h ${CLIENT_SOURCE_DIR}/samples/InterfaceBuilder/interface_builder.py
	COMMAND ${CMAKE_COMMAND} -E make_directory ${INCLUDE_TARGET_DIR}
	COMMAND ${Python3_EXECUTABLE} ${CLIENT_SOURCE_DIR}/samples/InterfaceBuilder/interface_builder.py
		${CLIENT_SOURCE_DIR}/gcCore.h ${INCLUDE_TARGET_DIR}/gcCoreAPI.h ${CLIENT_SOURCE_DIR}/gcCore.cpp
	COMMAND ${CMAKE_COMMAND} -E copy ${CLIENT_SOURCE_DIR}/gcGUI.h ${INCLUDE_TARGET_DIR}/gcGUI.h
	VERBATIM
)
add_custom_target(MetalClient_Interface
	DEPENDS ${INCLUDE_TARGET_DIR}/gcCoreAPI.h ${INCLUDE_TARGET_DIR}/gcGUI.h
)

install(FILES ${INCLUDE_TARGET_DIR}/gcCoreAPI.h ${INCLUDE_TARGET_DIR}/gcGUI.h DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/Orbitersdk/include)

if(NOT ORBITER_BUILD_METALCLIENT)
	return()
endif()

add_library(Metal9 STATIC
	Metal9/M9API.mm
	Metal9/M9D3DX.mm
	Metal9/M9Device.mm
	Metal9/M9Effect.mm
	Metal9/M9Resources.mm
)
target_include_directories(Metal9 PUBLIC
	${CLIENT_SOURCE_DIR}/Metal9
	${ORBITER_SOURCE_SDK_INCLUDE_DIR}
	${ORBITER_SOURCE_ROOT_DIR}/Src/Platform/include
)
target_compile_options(Metal9 PRIVATE -fobjc-arc)
set_target_properties(Metal9 PROPERTIES POSITION_INDEPENDENT_CODE ON FOLDER OVP)
target_link_libraries(Metal9 PUBLIC
	OrbiterPlatform
	"-framework Metal"
	"-framework QuartzCore"
	"-framework CoreText"
	"-framework CoreGraphics"
	"-framework ImageIO"
	"-framework UniformTypeIdentifiers"
	"-framework Foundation"
)

set(SourceFiles
	AABBUtil.cpp
	AtmoControls.cpp
	BeaconArray.cpp
	CelSphere.cpp
	CloudMgr.cpp
	Cloudmgr2.cpp
	CSphereMgr.cpp
	D3D9Client.cpp
	D3D9Config.cpp
	D3D9ControlPanel.cpp
	D3D9Effect.cpp
	D3D9Frame.cpp
	D3D9Pad.cpp
	D3D9Pad2.cpp
	D3D9Pad3.cpp
	D3D9Surface.cpp
	D3D9TextMgr.cpp
	D3D9Util.cpp
	DebugControls.cpp
	gcCore.cpp
	gcConst.cpp
	HazeMgr.cpp
	IProcess.cpp
	Log.cpp
	MaterialMgr.cpp
	Mesh.cpp
	MeshMgr.cpp
	OapiExtension.cpp
	Particle.cpp
	RingMgr.cpp
	RunwayLights.cpp
	Scene.cpp
	Spherepatch.cpp
	SurfMgr.cpp
	Surfmgr2.cpp
	TileLabel.cpp
	TileMgr.cpp
	Tilemgr2.cpp
	VBase.cpp
	VideoTab.cpp
	VObject.cpp
	VPlanet.cpp
	VPlanetAtmo.cpp
	VStar.cpp
	VVessel.cpp
	WindowMgr.cpp
	ZTreeMgr.cpp
	Tilemgr2_imp.hpp
	ClientImGui.mm
)


add_library(MetalClient SHARED
	${SourceFiles}
)

target_include_directories(MetalClient PRIVATE
	${CLIENT_SOURCE_DIR}
	${CMAKE_CURRENT_BINARY_DIR}
	${imgui_SOURCE_DIR}/
	${imgui_SOURCE_DIR}/backends/
)

target_link_libraries(MetalClient
	${ORBITER_LIB}
	${ORBITER_SDK_LIB}
	Metal9
)

set_target_properties(MetalClient
	PROPERTIES
	LIBRARY_OUTPUT_DIRECTORY ${ORBITER_BINARY_ROOT_DIR}/Modules/Plugin
	COMPILE_DEFINITIONS D3D9CLIENT_EXPORTS
	FOLDER OVP
)
set_source_files_properties(ClientImGui.mm PROPERTIES COMPILE_OPTIONS -fobjc-arc)

file(GLOB CLIENT_SHADERS CONFIGURE_DEPENDS ${CLIENT_SOURCE_DIR}/shaders/*)
set(CLIENT_SHADER_OUTPUTS)
foreach(shader ${CLIENT_SHADERS})
	get_filename_component(shader_name ${shader} NAME)
	set(shader_out ${ORBITER_BINARY_ROOT_DIR}/Modules/MetalClient/${shader_name})
	add_custom_command(
		OUTPUT ${shader_out}
		COMMAND ${CMAKE_COMMAND} -E copy_if_different ${shader} ${shader_out}
		DEPENDS ${shader}
	)
	list(APPEND CLIENT_SHADER_OUTPUTS ${shader_out})
endforeach()
add_custom_target(MetalClientShaders ALL DEPENDS ${CLIENT_SHADER_OUTPUTS})
add_dependencies(MetalClient MetalClientShaders)

add_dependencies(MetalClient
	${OrbiterTgt}
	Orbitersdk
	MetalClient_Interface
)

install(TARGETS MetalClient LIBRARY DESTINATION ${ORBITER_INSTALL_PLUGIN_DIR})

add_subdirectory(samples/DrawOrbits)
add_subdirectory(samples/DX9ExtMFD)
add_subdirectory(samples/GenericCamera)
add_subdirectory(samples/TerrainToolKit)
install(DIRECTORY ${CLIENT_SOURCE_DIR}/shaders/ DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/Modules/MetalClient)

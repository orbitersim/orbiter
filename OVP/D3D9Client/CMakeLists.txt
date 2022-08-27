
configure_file(D3D9ClientConfig.h.in D3D9ClientConfig.h)

# specify the C++ standard
# set(CMAKE_CXX_STANDARD 11)
# set(CMAKE_CXX_STANDARD_REQUIRED True)

# set the project name and version
project(D3D9Client VERSION 31.0)

# Check build platform
if(CMAKE_SIZEOF_VOID_P EQUAL 8) # 64-bit build?
	set(BUILD64 1)
	set(ARCH "x64")
else()
	set(BUILD64 0)
	set(ARCH "x86")
endif()

# Location of DirectX SDK
# set(DXSDK_DIR "C:/Program Files (x86)/Microsoft DirectX SDK (June 2010)")

if (NOT "$ENV{DXSDK_DIR}" STREQUAL "")
	file(TO_CMAKE_PATH "$ENV{DXSDK_DIR}" DXSDK_DIR)
else()
    set(DXSDK_DIR "C:/Program Files (x86)/Microsoft DirectX SDK (June 2010)")
endif()
set(DXSDK_LIB_DIR ${DXSDK_DIR}/lib/${ARCH})

set(ORBITER_SOURCE_SDK_INCLUDE_DIR "${CMAKE_BINARY_DIR}/Orbitersdk/include" "${CMAKE_SOURCE_DIR}/Orbitersdk/include")
set(ORBITER_SOURCE_SDK_LIB_DIR ${CMAKE_BINARY_DIR}/Orbitersdk/lib)

set(D3D9_SOURCE_DIR ${CMAKE_SOURCE_DIR}/OVP/D3D9Client)
set(ShaderDir ${D3D9_SOURCE_DIR}/shaders)
set(IncludeDir ${CMAKE_SOURCE_DIR}/Orbitersdk/include)

set(InterfaceBuilder ${D3D9_SOURCE_DIR}/samples/InterfaceBuilder/InterfaceBuilder.exe)

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
	GDIPad.cpp
	HazeMgr.cpp
	IProcess.cpp
	Junction.cpp
	Log.cpp
	MaterialMgr.cpp
	Mesh.cpp
	MeshMgr.cpp
	OapiExtension.cpp
	Particle.cpp
	PlanetRenderer.cpp
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
	VStar.cpp
	VVessel.cpp
	WindowMgr.cpp
	ZTreeMgr.cpp
	Tilemgr2_imp.hpp
)

set(IncludeFiles
	AABBUtil.h
	AtmoControls.h
	BeaconArray.h
	CelSphere.h
	CloudMgr.h
	Cloudmgr2.h
	CSphereMgr.h
	D3D9Catalog.h
	D3D9Client.h
	D3D9Config.h
	D3D9Effect.h
	D3D9Frame.h
	D3D9Pad.h
	D3D9Surface.h
	D3D9TextMgr.h
	D3D9Util.h
	DebugControls.h
	GDIPad.h
	HazeMgr.h
	IProcess.h
	Junction.h
	Log.h
	MaterialMgr.h
	Mesh.h
	MeshMgr.h
	OapiExtension.h
	Particle.h
	PlanetRenderer.h
	Qtree.h
	resource.h
	RingMgr.h
	RunwayLights.h
	Scene.h
	Spherepatch.h
	SurfMgr.h
	Surfmgr2.h
	TileLabel.h
	TileMgr.h
	Tilemgr2.h
	VBase.h
	VectorHelpers.h
	VideoTab.h
	VObject.h
	VPlanet.h
	VStar.h
	VVessel.h
	WindowMgr.h
	ZTreeMgr.h
	gcConst.h
	gcCore.h
)

set(APIHeaders
    ${IncludeDir}/gcGUI.h
	${IncludeDir}/gcCoreAPI.h
)

set(ShaderFiles
	${ShaderDir}/BeaconArray.fx
	${ShaderDir}/CKBlit.fx
	${ShaderDir}/Common.hlsl
	${ShaderDir}/D3D9Client.fx
	${ShaderDir}/EnvMapBlur.hlsl
	${ShaderDir}/GDIOverlay.hlsl
	${ShaderDir}/HorizonHaze.fx
	${ShaderDir}/IPI.hlsl
	${ShaderDir}/IrradianceInteg.hlsl
	${ShaderDir}/LensFlare.hlsl
	${ShaderDir}/LightBlur.hlsl
	${ShaderDir}/Mesh.fx
	${ShaderDir}/Metalness.fx
	${ShaderDir}/Particle.fx
	${ShaderDir}/PBR.fx
	${ShaderDir}/Planet.fx
	${ShaderDir}/SceneTech.fx
	${ShaderDir}/Sketchpad.fx
	${ShaderDir}/Surface.fx
	${ShaderDir}/Vessel.fx
)

source_group(APIHeaders FILES ${APIHeaders})

source_group(Shaders FILES ${ShaderFiles})

set_property(SOURCE ${ShaderFiles}
    PROPERTY VS_SETTINGS "ExcludedFromBuild=true"
)

add_library(D3D9Client MODULE
	${SourceFiles}
	${IncludeFiles}
    ${APIHeaders}
	${ShaderFiles}
	D3D9Client.rc
)

target_include_directories(D3D9Client PUBLIC
	${ORBITER_SOURCE_SDK_INCLUDE_DIR}
	${DXSDK_DIR}/Include
)

target_link_directories(D3D9Client PUBLIC
	${ORBITER_SOURCE_SDK_LIB_DIR}
	${DXSDK_LIB_DIR}
)

target_link_libraries(D3D9Client
	orbiter.lib
	orbitersdk.lib
	d3d9.lib
	debug d3dx9d.lib
	optimized d3dx9.lib
	kernel32.lib
	user32.lib
	gdi32.lib
	winspool.lib
	comdlg32.lib
	advapi32.lib
	shell32.lib
	ole32.lib
	oleaut32.lib
	uuid.lib
	odbc32.lib
	odbccp32.lib
	version.lib
	msimg32.lib
)

set_target_properties(D3D9Client
	PROPERTIES
	COMPILE_FLAGS "${CompileFlags}"
	LIBRARY_OUTPUT_DIRECTORY_DEBUG ${CMAKE_BINARY_DIR}/Modules/Plugin
	LIBRARY_OUTPUT_DIRECTORY_RELEASE ${CMAKE_BINARY_DIR}/Modules/Plugin
	COMPILE_DEFINITIONS D3D9CLIENT_EXPORTS
	FOLDER OVP
)

add_custom_command(
	TARGET D3D9Client PRE_BUILD
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${D3D9_SOURCE_DIR}/shaders/ ${ORBITER_BINARY_MODULE_DIR}/D3D9Client
)

add_custom_command(OUTPUT ${IncludeDir}/gcCoreAPI.h
	WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
	COMMAND ${InterfaceBuilder} ${D3D9_SOURCE_DIR}/gcCore.h ${IncludeDir}/gcCoreAPI.h ${D3D9_SOURCE_DIR}/gcCore.cpp
	VERBATIM
)
add_custom_target(D3D9Client_Interface
	DEPENDS ${IncludeDir}/gcCoreAPI.h
)

add_dependencies(D3D9Client
	${OrbiterTgt}
	Orbitersdk
	D3D9Client_Interface
)

install(TARGETS D3D9Client
	LIBRARY	
	DESTINATION ${ORBITER_INSTALL_PLUGIN_DIR}
)

add_subdirectory(samples/DrawOrbits)
add_subdirectory(samples/DX9ExtMFD)
add_subdirectory(samples/GenericCamera)
add_subdirectory(samples/TerrainToolBox)

install(DIRECTORY ${CMAKE_SOURCE_DIR}/Meshes/ DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/Meshes)
install(DIRECTORY ${CMAKE_SOURCE_DIR}/Config/ DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/Config)
install(DIRECTORY ${CMAKE_SOURCE_DIR}/Textures/ DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/Textures)
install(DIRECTORY ${CMAKE_SOURCE_DIR}/Orbitersdk/doc/ DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/Orbitersdk/doc)
install(DIRECTORY ${D3D9_SOURCE_DIR}/shaders/ DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/Modules/D3D9Client)
install(DIRECTORY ${D3D9_SOURCE_DIR}/samples/DrawOrbits/ DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/Orbitersdk/samples/DrawOrbits)
install(DIRECTORY ${D3D9_SOURCE_DIR}/samples/DX9ExtMFD/ DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/Orbitersdk/samples/DX9ExtMFD)
install(DIRECTORY ${D3D9_SOURCE_DIR}/samples/GenericCamera/ DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/Orbitersdk/samples/GenericCamera)
install(DIRECTORY ${D3D9_SOURCE_DIR}/samples/TerrainToolBox/ DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/Orbitersdk/samples/TerrainToolBox)
install(FILES ${IncludeDir}/gcCoreAPI.h DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/Orbitersdk/include)
install(FILES ${IncludeDir}/gcGUI.h DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/Orbitersdk/include)


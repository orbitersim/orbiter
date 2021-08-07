
configure_file(D3D9ClientConfig.h.in D3D9ClientConfig.h)

# specify the C++ standard
# set(CMAKE_CXX_STANDARD 11)
# set(CMAKE_CXX_STANDARD_REQUIRED True)

set(ShaderDir ${CMAKE_SOURCE_DIR}/Modules/D3D9Client)

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
	FileParser.cpp
	gcCore.cpp
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
	Texture.cpp
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
	FileParser.h
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
	Texture.h
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

add_library(D3D9Client MODULE
	${SourceFiles}
	${IncludeFiles}
	${ShaderFiles}
	D3D9Client.rc
)

target_include_directories(D3D9Client PUBLIC
	${ORBITER_SOURCE_SDK_INCLUDE_DIR}
	${DXSDK_DIR}/Include
)

target_link_directories(D3D9Client PUBLIC
	${ORBITER_SOURCE_SDK_LIB_DIR}
	${DXSDK_DIR}/Lib/x64
)

target_link_libraries(D3D9Client
	orbiter.lib
	orbitersdk.lib
	d3d9.lib
	d3dx9d.lib
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
	LINK_FLAGS "/SUBSYSTEM:WINDOWS"
	LIBRARY_OUTPUT_DIRECTORY_DEBUG ${CMAKE_BINARY_DIR}/Modules/Plugin
	LIBRARY_OUTPUT_DIRECTORY_RELEASE ${CMAKE_BINARY_DIR}/Modules/Plugin
	COMPILE_DEFINITIONS D3D9CLIENT_EXPORTS
)

add_custom_command(
	TARGET D3D9Client POST_BUILD
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${ShaderDir}/ ${CMAKE_BINARY_DIR}/Modules/D3D9Client
)

install(TARGETS D3D9Client
	LIBRARY	
	DESTINATION Modules/Plugin
)



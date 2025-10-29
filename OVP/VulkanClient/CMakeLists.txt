# set the project name and version
project(VulkanClient VERSION 0.0)

configure_file(ClientConfig.h.in ClientConfig.h)

# specify the C++ standard
# set(CMAKE_CXX_STANDARD 11)
# set(CMAKE_CXX_STANDARD_REQUIRED True)

set(DXSDK_LIB_DIR ${DXSDK_DIR}/lib/${ARCH})
set(DXSDK_LIB_DIR2 "C:/Program Files (x86)/Windows Kits/10/Lib/10.0.19041.0/um/${ARCH}")  #Must acquire d3d9.lib from here

set(VULKAN_SOURCE_DIR ${CMAKE_SOURCE_DIR}/OVP/VulkanClient)
set(ShaderDir ${VULKAN_SOURCE_DIR}/shaders)
set(INCLUDE_TARGET_DIR ${ORBITER_BINARY_SDK_DIR}/include)

set(InterfaceBuilder ${ORBITER_SOURCE_ROOT_DIR}/OVP/D3D9Client/samples/InterfaceBuilder/InterfaceBuilder.exe)

if (${ARCH} STREQUAL "x64")
	set(VULKAN_LIB_DIR "${VULKAN_DIR}/Lib")
else()
	set(VULKAN_LIB_DIR "${VULKAN_DIR}/Lib32")
endif()

set(SourceFiles
	AABBUtil.cpp
	AtmoControls.cpp
	BeaconArray.cpp
	CelSphere.cpp
	CloudMgr.cpp
	Cloudmgr2.cpp
	CSphereMgr.cpp
	Client.cpp
	Config.cpp
	ControlPanel.cpp
	Effect.cpp
	Frame.cpp
	Pad.cpp
	Pad2.cpp
	Pad3.cpp
	Surface.cpp
	TextMgr.cpp
	Util.cpp
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
)

set(IncludeFiles
	AABBUtil.h
	AtmoControls.h
	BeaconArray.h
	CelSphere.h
	CloudMgr.h
	Cloudmgr2.h
	CSphereMgr.h
	Catalog.h
	Client.h
	Config.h
	Effect.h
	Frame.h
	Pad.h
	Surface.h
	TextMgr.h
	Util.h
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
    ${INCLUDE_TARGET_DIR}/gcGUI.h
	${INCLUDE_TARGET_DIR}/gcCoreAPI.h
	${CMAKE_SOURCE_DIR}/Orbitersdk/include/MathAPI.h
	${CMAKE_SOURCE_DIR}/Orbitersdk/include/DrawAPI.h
	${CMAKE_SOURCE_DIR}/Orbitersdk/include/OrbiterAPI.h
	${CMAKE_SOURCE_DIR}/Orbitersdk/include/GraphicsAPI.h
	${EXTERN_DIR}/dxmath/Inc/DirectXMath.h
	${EXTERN_DIR}/dxmath/Inc/DirectXCollision.h
	${VULKAN_DIR}/Include/vulkan/vulkan.hpp
	${VULKAN_DIR}/Include/vulkan/vk_sdk_platform.h
)

set(ShaderFiles
	${ShaderDir}/BeaconArray.fx
	${ShaderDir}/Common.hlsl
	${ShaderDir}/vkClient.fx
	${ShaderDir}/EnvMapBlur.hlsl
	${ShaderDir}/GDIOverlay.hlsl
	${ShaderDir}/HorizonHaze.fx
	${ShaderDir}/IPI.hlsl
	${ShaderDir}/IrradianceInteg.hlsl
	${ShaderDir}/LensFlare.hlsl
	${ShaderDir}/LightBlur.hlsl
    ${ShaderDir}/CelSphere.hlsl
    ${ShaderDir}/NewMesh.hlsl
    ${ShaderDir}/NewPlanet.hlsl
    ${ShaderDir}/Scatter.hlsl
    ${ShaderDir}/Glare.hlsl
    ${ShaderDir}/PreBakeLights.hlsl
    ${ShaderDir}/Custom.hlsl
	${ShaderDir}/Mesh.fx
	${ShaderDir}/Metalness.fx
	${ShaderDir}/Particle.fx
	${ShaderDir}/PBR.fx
	${ShaderDir}/Planet.fx
	${ShaderDir}/SceneTech.fx
	${ShaderDir}/Sketchpad.fx
	${ShaderDir}/Vessel.fx
    ${ShaderDir}/BakedVC.fx
)

source_group(APIHeaders FILES ${APIHeaders})

source_group(Shaders FILES ${ShaderFiles})

set_property(SOURCE ${ShaderFiles}
    PROPERTY VS_SETTINGS "ExcludedFromBuild=true"
)

add_library(VulkanClient MODULE
	${SourceFiles}
	${IncludeFiles}
    ${APIHeaders}
	${ShaderFiles}
	Client.rc
)

target_include_directories(VulkanClient PUBLIC
	${ORBITER_SOURCE_SDK_INCLUDE_DIR}
	${DXSDK_DIR}/Include
	${EXTERN_DIR}/dxmath/Inc
	${VULKAN_DIR}/Include
)

target_link_directories(VulkanClient PUBLIC
	${ORBITER_BINARY_SDK_DIR}/lib
	${DXSDK_LIB_DIR2}
	${DXSDK_LIB_DIR}
	${VULKAN_LIB_DIR}
)

target_link_libraries(VulkanClient
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

set_target_properties(VulkanClient
	PROPERTIES
	LIBRARY_OUTPUT_DIRECTORY_DEBUG ${CMAKE_BINARY_DIR}/Modules/Plugin
	LIBRARY_OUTPUT_DIRECTORY_RELEASE ${CMAKE_BINARY_DIR}/Modules/Plugin
	COMPILE_DEFINITIONS vkCLIENT_EXPORTS
	FOLDER OVP
)

add_custom_command(
	TARGET VulkanClient PRE_BUILD
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${VULKAN_SOURCE_DIR}/shaders/ ${ORBITER_BINARY_MODULE_DIR}/vkShaders
)

add_custom_command(OUTPUT ${INCLUDE_TARGET_DIR}/gcCoreAPI.h ${INCLUDE_TARGET_DIR}/gcGUI.h
	DEPENDS ${VULKAN_SOURCE_DIR}/gcGUI.h ${VULKAN_SOURCE_DIR}/gcCore.h ${VULKAN_SOURCE_DIR}/gcCore.cpp
	COMMAND ${InterfaceBuilder} ${VULKAN_SOURCE_DIR}/gcCore.h ${INCLUDE_TARGET_DIR}/gcCoreAPI.h ${VULKAN_SOURCE_DIR}/gcCore.cpp
	COMMAND ${CMAKE_COMMAND} -E copy_if_different ${VULKAN_SOURCE_DIR}/gcGUI.h ${INCLUDE_TARGET_DIR}/gcGUI.h
	VERBATIM
)

add_dependencies(VulkanClient
	${OrbiterTgt}
	Orbitersdk
	D3D9Client_Interface
)

install(TARGETS VulkanClient
	LIBRARY	
	DESTINATION ${ORBITER_INSTALL_PLUGIN_DIR}
)

# add_subdirectory(samples/DrawOrbits)
# add_subdirectory(samples/DX9ExtMFD)
# add_subdirectory(samples/GenericCamera)
# add_subdirectory(samples/TerrainToolKit)

set(Exclude
	PATTERN CMakeLists.txt EXCLUDE
)

install(DIRECTORY ${VULKAN_SOURCE_DIR}/shaders/ DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/Modules/vkShaders)
# install(DIRECTORY ${VULKAN_SOURCE_DIR}/samples/DrawOrbits/ DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/Orbitersdk/samples/DrawOrbits ${Exclude})
# install(DIRECTORY ${VULKAN_SOURCE_DIR}/samples/DX9ExtMFD/ DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/Orbitersdk/samples/DX9ExtMFD ${Exclude})
# install(DIRECTORY ${VULKAN_SOURCE_DIR}/samples/GenericCamera/ DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/Orbitersdk/samples/GenericCamera ${Exclude})
install(FILES ${INCLUDE_TARGET_DIR}/gcCoreAPI.h DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/Orbitersdk/include)
install(FILES ${INCLUDE_TARGET_DIR}/gcGUI.h DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/Orbitersdk/include)

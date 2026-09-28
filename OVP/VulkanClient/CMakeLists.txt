# set the project name and version
project(D3D9Client VERSION 31.0)

configure_file(D3D9ClientConfig.h.in D3D9ClientConfig.h)
FetchContent_MakeAvailable(imgui)

# Vulkan Memory Allocator replaces the D3DPOOL resource management (header only)
FetchContent_Declare(vma
	GIT_REPOSITORY https://github.com/GPUOpen-LibrariesAndSDKs/VulkanMemoryAllocator.git
	GIT_TAG v3.4.0
)
FetchContent_MakeAvailable(vma)

# glslang compiles the shaders at run time, as D3DX did
find_package(glslang REQUIRED)

# C++20 as the rest of the port (upstream: C++17)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED True)

# DXSDK library paths left out: Vulkan, VMA and glslang replace d3d9/d3dx9

set(D3D9_SOURCE_DIR ${CMAKE_SOURCE_DIR}/OVP/VulkanClient)
set(ShaderDir ${D3D9_SOURCE_DIR}/shaders)
set(INCLUDE_TARGET_DIR ${ORBITER_BINARY_SDK_DIR}/include)

set(InterfaceBuilder ${Python3_EXECUTABLE} ${D3D9_SOURCE_DIR}/samples/InterfaceBuilder/InterfaceBuilder.py) # InterfaceBuilder.exe is Windows only

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
	${imgui_SOURCE_DIR}/backends/imgui_impl_vulkan.cpp # imgui_impl_dx9
	D3DXMath.cpp # not upstream: D3DX math types and functions
	VkCore.cpp # not upstream: Vulkan device, buffers, textures
	VkShader.cpp # not upstream: GLSL compiler, constant tables, effects
	VkTexFile.cpp # not upstream: texture files, pixel formats, readbacks
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
	D3DXMath.h # not upstream
	VkCore.h # not upstream
	VkShader.h # not upstream
	VkTexFile.h # not upstream
)

set(APIHeaders
    ${INCLUDE_TARGET_DIR}/gcGUI.h
	${INCLUDE_TARGET_DIR}/gcCoreAPI.h
)

set(ShaderFiles
	${ShaderDir}/BeaconArray.fx
	${ShaderDir}/Common.glsl
	${ShaderDir}/D3D9Client.fx
	${ShaderDir}/EnvMapBlur.glsl
	${ShaderDir}/GDIOverlay.glsl
	${ShaderDir}/HorizonHaze.fx
	${ShaderDir}/IPI.glsl
	${ShaderDir}/IrradianceInteg.glsl
	${ShaderDir}/LensFlare.glsl
	${ShaderDir}/LightBlur.glsl
    ${ShaderDir}/CelSphere.glsl
    ${ShaderDir}/NewMesh.glsl
    ${ShaderDir}/NewPlanet.glsl
    ${ShaderDir}/Scatter.glsl
    ${ShaderDir}/Glare.glsl
	${ShaderDir}/Mesh.fx
	${ShaderDir}/Metalness.fx
	${ShaderDir}/Particle.fx
	${ShaderDir}/PBR.fx
	${ShaderDir}/Planet.fx
	${ShaderDir}/SceneTech.fx
	${ShaderDir}/Sketchpad.fx
	${ShaderDir}/Vessel.fx
)

source_group(Shaders FILES ${ShaderFiles})

# D3D9Client.rc: the dialogs are built as Qt widgets from the compiled resource table (rc2cpp)
add_library(D3D9Client MODULE
	${SourceFiles}
	${IncludeFiles}
	${ShaderFiles}
	${APIHeaders}
)
orbiter_rc_resources(D3D9Client D3D9Client.rc)

target_include_directories(D3D9Client PUBLIC
	${ORBITER_SOURCE_SDK_INCLUDE_DIR}
	${D3D9_SOURCE_DIR}
	${CMAKE_CURRENT_BINARY_DIR}
	${imgui_SOURCE_DIR}/
	${imgui_SOURCE_DIR}/backends/
	${vma_SOURCE_DIR}/include
)

# orbiter symbols resolve against the executable at dlopen
target_link_libraries(D3D9Client
	Orbitersdk
	Vulkan::Vulkan
	Qt6::Widgets
	Qt6::DBus
	glslang::glslang
	glslang::SPIRV
	glslang::glslang-default-resource-limits
)

set_target_properties(D3D9Client
	PROPERTIES
	OUTPUT_NAME VulkanClient
	PREFIX ""
	LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/Modules/Plugin
	COMPILE_DEFINITIONS D3D9CLIENT_EXPORTS
	LINK_OPTIONS "-Wl,-soname,VulkanClient.so" # gcCore.h/gcGUI.h find the loaded client by it
	FOLDER OVP
)

add_custom_command(
	TARGET D3D9Client PRE_BUILD
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${D3D9_SOURCE_DIR}/shaders/ ${ORBITER_BINARY_MODULE_DIR}/VulkanClient
)

add_custom_command(OUTPUT ${INCLUDE_TARGET_DIR}/gcCoreAPI.h ${INCLUDE_TARGET_DIR}/gcGUI.h
	DEPENDS ${D3D9_SOURCE_DIR}/gcGUI.h ${D3D9_SOURCE_DIR}/gcCore.h ${D3D9_SOURCE_DIR}/gcCore.cpp ${D3D9_SOURCE_DIR}/samples/InterfaceBuilder/InterfaceBuilder.py
	COMMAND ${InterfaceBuilder} ${D3D9_SOURCE_DIR}/gcCore.h ${INCLUDE_TARGET_DIR}/gcCoreAPI.h ${D3D9_SOURCE_DIR}/gcCore.cpp
	COMMAND ${CMAKE_COMMAND} -E copy ${D3D9_SOURCE_DIR}/gcGUI.h ${INCLUDE_TARGET_DIR}/gcGUI.h
	VERBATIM
)
add_custom_target(D3D9Client_Interface
	DEPENDS ${INCLUDE_TARGET_DIR}/gcCoreAPI.h ${INCLUDE_TARGET_DIR}/gcGUI.h
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
add_subdirectory(samples/TerrainToolKit)

set(Exclude
	PATTERN CMakeLists.txt EXCLUDE
)

install(DIRECTORY ${D3D9_SOURCE_DIR}/shaders/ DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/Modules/VulkanClient)
install(DIRECTORY ${D3D9_SOURCE_DIR}/samples/DrawOrbits/ DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/Orbitersdk/samples/DrawOrbits ${Exclude})
install(DIRECTORY ${D3D9_SOURCE_DIR}/samples/DX9ExtMFD/ DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/Orbitersdk/samples/DX9ExtMFD ${Exclude})
install(DIRECTORY ${D3D9_SOURCE_DIR}/samples/GenericCamera/ DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/Orbitersdk/samples/GenericCamera ${Exclude})
install(FILES ${INCLUDE_TARGET_DIR}/gcCoreAPI.h DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/Orbitersdk/include)
install(FILES ${INCLUDE_TARGET_DIR}/gcGUI.h DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/Orbitersdk/include)

# not upstream: the client's manual goes to Doc/VulkanClient
install(FILES ${D3D9_SOURCE_DIR}/doc/VulkanClient.html DESTINATION ${ORBITER_INSTALL_DOC_DIR}/VulkanClient)
install(FILES
	"${D3D9_SOURCE_DIR}/doc/images/Video Tab.png"
	"${D3D9_SOURCE_DIR}/doc/images/VulkanClient Advanced Setup.png"
	"${D3D9_SOURCE_DIR}/doc/images/Vulkan Graphics Controls.png"
	"${D3D9_SOURCE_DIR}/doc/images/Vulkan Atmospheric Controls.png"
	"${D3D9_SOURCE_DIR}/doc/images/Vulkan Debug Controls.png"
	${D3D9_SOURCE_DIR}/doc/images/PickInAction.png
	${D3D9_SOURCE_DIR}/doc/images/rwylights.png
	DESTINATION ${ORBITER_INSTALL_DOC_DIR}/VulkanClient/images
)

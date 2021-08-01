# Copyright (c) Martin Schweiger
# Licensed under the MIT License

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${ORBITER_BINARY_PLUGIN_DIR})

# SAFESEH linker flag must be turned off because the DX7 libraries don't support it'
set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -SAFESEH:NO")

add_library(D3D7Client SHARED
	Camera.cpp
	CelSphere.cpp
	CloudMgr.cpp
	cloudmgr2.cpp
	CSphereMgr.cpp
	D3D7Client.cpp
	D3D7Client.rc
	D3D7Config.cpp
	D3D7Enum.cpp
	D3D7Extra.cpp
	D3D7Frame.cpp
	D3D7Util.cpp
	HazeMgr.cpp
	Light.cpp
	Log.cpp
	Mesh.cpp
	MeshMgr.cpp
	Particle.cpp
	RingMgr.cpp
	Scene.cpp
	spherepatch.cpp
	SurfMgr.cpp
	surfmgr2.cpp
	Texture.cpp
	tilelabel.cpp
	TileMgr.cpp
	tilemgr2.cpp
	VBase.cpp
	VideoTab.cpp
	VObject.cpp
	VPlanet.cpp
	VStar.cpp
	VVessel.cpp
	ztreemgr.cpp
)

target_include_directories(D3D7Client
	PUBLIC ${CMAKE_SOURCE_DIR}/Orbitersdk/include
	PUBLIC ${DX7SDK_INCLUDE_DIR}
)

add_dependencies(D3D7Client
	${OrbiterTgt}
	Orbitersdk
	GDIClient
)

target_link_libraries(D3D7Client
	${ORBITER_LIB}
	${ORBITER_SDK_LIB}
	${GDICLIENT_LIB}
	dxguid.lib
	${DX7SDK_LIB_DIR}/d3dim.lib
	${DX7SDK_LIB_DIR}/ddraw.lib
)

set_target_properties(D3D7Client
	PROPERTIES
	COMPILE_DEFINITIONS "DIRECTINPUT_VERSION=0x0800"
	FOLDER OVP
)

# Installation
install(TARGETS
	D3D7Client
	RUNTIME
	DESTINATION ${ORBITER_INSTALL_PLUGIN_DIR}
)

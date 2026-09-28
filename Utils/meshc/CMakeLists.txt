# Copyright (c) Martin Schweiger
# Licensed under the MIT License

add_executable(meshc
	meshc.cpp
	Mesh.cpp
)

if(WIN32) # Windows: upstream
target_include_directories(meshc
	PUBLIC ${ORBITER_SOURCE_DIR}
)
else() # Linux port
target_include_directories(meshc
	PUBLIC ${ORBITER_SOURCE_DIR}
	PUBLIC ${ORBITER_SOURCE_SDK_INCLUDE_DIR} # not upstream: D3D7 types became the SDK's NTVERTEX/MATERIAL/FVECTOR3
)
endif() # WIN32

set_target_properties(meshc
	PROPERTIES
	FOLDER Tools
)

install(TARGETS meshc
	DESTINATION ${ORBITER_INSTALL_SDK_DIR}/Utils
)

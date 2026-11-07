# Copyright (c) Martin Schweiger
# Licensed under the MIT License

add_executable(meshc
	meshc.cpp
	Mesh.cpp
)

target_include_directories(meshc
	PUBLIC ${ORBITER_SOURCE_DIR}
)

set_target_properties(meshc
	PROPERTIES
	FOLDER Tools
	INSTALL_RPATH "@executable_path/../.."
)

install(TARGETS meshc
	DESTINATION ${ORBITER_INSTALL_SDK_DIR}/Utils
)

target_link_libraries(meshc OrbiterPlatform)

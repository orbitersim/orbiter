# Copyright (c) Martin Schweiger
# Licensed under the MIT License

add_executable(Shipedit
	Shipedit.cpp
	Mesh.cpp
	Vecmat.cpp
)

target_include_directories(Shipedit
	PRIVATE ${ORBITER_SOURCE_DIR}
)

target_link_libraries(Shipedit UtilsImGui)

set_target_properties(Shipedit
	PROPERTIES
	FOLDER Tools
	INSTALL_RPATH "@executable_path/..;@executable_path/../.."
)

install(TARGETS
	Shipedit
	RUNTIME
	DESTINATION ${ORBITER_INSTALL_SDK_DIR}/Utils
)

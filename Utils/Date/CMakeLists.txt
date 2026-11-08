# Copyright (c) Martin Schweiger
# Licensed under the MIT License

add_executable(Date
	Date.cpp
	Convert.cpp
)

target_link_libraries(Date UtilsImGui)

set_target_properties(Date
	PROPERTIES
	FOLDER Tools
	INSTALL_RPATH "@executable_path/..;@executable_path/../.."
)

install(TARGETS
	Date
	RUNTIME
	DESTINATION ${ORBITER_INSTALL_UTILS_DIR}
)

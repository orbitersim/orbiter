# Copyright (c) Martin Schweiger
# Licensed under the MIT License

add_executable(plsplit
	plsplit.cpp
)

target_link_libraries(plsplit UtilsDDS OrbiterPlatform)

set_target_properties(plsplit
	PROPERTIES
	FOLDER Tools
	INSTALL_RPATH "@executable_path/..;@executable_path/../.."
)

install(TARGETS
	plsplit
	RUNTIME
	DESTINATION ${ORBITER_INSTALL_UTILS_DIR}
)

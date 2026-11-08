# Copyright (c) Martin Schweiger
# Licensed under the MIT License

add_executable(pltex
	Pltex.cpp
)

target_link_libraries(pltex UtilsDDS OrbiterPlatform)

set_target_properties(pltex
	PROPERTIES
	FOLDER Tools
	INSTALL_RPATH "@executable_path/..;@executable_path/../.."
)

install(TARGETS
	pltex
	RUNTIME
	DESTINATION ${ORBITER_INSTALL_UTILS_DIR}
)

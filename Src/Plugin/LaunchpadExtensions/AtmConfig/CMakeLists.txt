# Copyright (c) Martin Schweiger
# Licensed under the MIT License

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${ORBITER_BINARY_STARTUP_DIR})

add_library(AtmConfig SHARED
	AtmConfig.cpp
	AtmConfig.rc
)

target_include_directories(AtmConfig
	PUBLIC ${ORBITER_SOURCE_SDK_INCLUDE_DIR}
	PUBLIC ${MODULE_COMMON_DIR}
)

target_link_libraries(AtmConfig
	${ORBITER_LIB}
	${ORBITER_SDK_LIB}
)

add_dependencies(AtmConfig
	${OrbiterTgt}
	Orbitersdk
)

set_target_properties(AtmConfig
	PROPERTIES
	FOLDER Modules
)

# Installation
install(TARGETS AtmConfig
	RUNTIME
	DESTINATION ${ORBITER_INSTALL_MODULE_DIR}/Startup
)

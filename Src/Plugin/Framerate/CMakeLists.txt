# Copyright (c) Martin Schweiger
# Licensed under the MIT License

add_library(Framerate SHARED
	Framerate.cpp
)

target_include_directories(Framerate
	PUBLIC ${ORBITER_SOURCE_SDK_INCLUDE_DIR}
	PUBLIC ${MODULE_COMMON_DIR}
)

target_link_libraries(Framerate
	${ORBITER_LIB}
	${ORBITER_SDK_LIB}
	imgui
)

add_dependencies(Framerate
	${OrbiterTgt}
	Orbitersdk
)

set_target_properties(Framerate
	PROPERTIES
	FOLDER Modules
)

# Installation
install(TARGETS Framerate
	RUNTIME
	DESTINATION ${ORBITER_INSTALL_PLUGIN_DIR}
)

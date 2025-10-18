# Copyright (c) Martin Schweiger
# Licensed under the MIT License

add_library(ExtMFD SHARED
	ExtMFD.cpp
	MFDWindow.cpp
)

target_include_directories(ExtMFD
	PUBLIC ${ORBITER_SOURCE_SDK_INCLUDE_DIR}
	PUBLIC ${MODULE_COMMON_DIR}
)

target_link_libraries(ExtMFD
	${ORBITER_LIB}
	${ORBITER_SDK_LIB}
	imgui
)

add_dependencies(ExtMFD
	${OrbiterTgt}
	Orbitersdk
	imgui
)

set_target_properties(ExtMFD
	PROPERTIES
	FOLDER Modules
)

# Installation
install(TARGETS ExtMFD
	RUNTIME
	DESTINATION ${ORBITER_INSTALL_PLUGIN_DIR}
)

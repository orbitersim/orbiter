# Copyright (c) Martin Schweiger
# Licensed under the MIT License

add_library(TrackIR SHARED
	TrackIR.cpp
	TrackIR.rc
	TrackIRconfig.cpp
	NPClientWraps.cpp
)

target_include_directories(TrackIR
	PUBLIC ${ORBITER_SOURCE_SDK_INCLUDE_DIR}
	PUBLIC ${MODULE_COMMON_DIR}
)

target_link_libraries(TrackIR
	${ORBITER_LIB}
	${ORBITER_SDK_LIB}
	UxTheme.lib
)

add_dependencies(TrackIR
	Orbiter
	Orbitersdk
)

set_target_properties(TrackIR
	PROPERTIES
	FOLDER Modules
)

# Installation
install(TARGETS TrackIR
	RUNTIME
	DESTINATION ${ORBITER_INSTALL_PLUGIN_DIR}
)

# Copyright (c) Martin Schweiger
# Licensed under the MIT License

if(WIN32) # Windows: upstream
add_library(TrackIR SHARED
	TrackIR.cpp
	TrackIR.rc
	TrackIRconfig.cpp
	NPClientWraps.cpp
)
else() # Linux port
add_library(TrackIR SHARED
	TrackIR.cpp
	TrackIRconfig.cpp
	NPClientWraps.cpp
)
endif() # WIN32
if(NOT WIN32) # Linux port
orbiter_rc_resources(TrackIR TrackIR.rc)
endif() # NOT WIN32

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
	${OrbiterTgt}
	Orbitersdk
)

set_target_properties(TrackIR
	PROPERTIES
	FOLDER Modules
)

# Installation
if(WIN32) # Windows: upstream
install(TARGETS TrackIR
	RUNTIME
	DESTINATION ${ORBITER_INSTALL_PLUGIN_DIR}
)
else() # Linux port
install(TARGETS TrackIR
	LIBRARY
	DESTINATION ${ORBITER_INSTALL_PLUGIN_DIR}
)
endif() # WIN32

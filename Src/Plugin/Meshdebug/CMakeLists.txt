# Copyright (c) Martin Schweiger
# Licensed under the MIT License

if(WIN32) # Windows: upstream
add_library(Meshdebug SHARED
	Meshdebug.cpp
	Meshdebug.rc
)
else() # Linux port
add_library(Meshdebug SHARED
	Meshdebug.cpp
)
endif() # WIN32
if(NOT WIN32) # Linux port
orbiter_rc_resources(Meshdebug Meshdebug.rc)
endif() # NOT WIN32

target_include_directories(Meshdebug
	PUBLIC ${ORBITER_SOURCE_SDK_INCLUDE_DIR}
	PUBLIC ${MODULE_COMMON_DIR}
)

target_link_libraries(Meshdebug
	${ORBITER_LIB}
	${ORBITER_SDK_LIB}
)

add_dependencies(Meshdebug
	${OrbiterTgt}
	Orbitersdk
)

set_target_properties(Meshdebug
	PROPERTIES
	FOLDER Modules
)

# Installation
if(WIN32) # Windows: upstream
install(TARGETS Meshdebug
	RUNTIME
	DESTINATION ${ORBITER_INSTALL_PLUGIN_DIR}
)
else() # Linux port
install(TARGETS Meshdebug
	LIBRARY
	DESTINATION ${ORBITER_INSTALL_PLUGIN_DIR}
)
endif() # WIN32

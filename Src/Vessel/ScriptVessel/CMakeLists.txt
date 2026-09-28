# Copyright (c) Martin Schweiger
# Licensed under the MIT License

add_library(ScriptVessel SHARED
	ScriptVessel.cpp
)

set_target_properties(ScriptVessel
	PROPERTIES
	FOLDER Vessels
)

target_include_directories(ScriptVessel
	PUBLIC ${ORBITER_SOURCE_SDK_INCLUDE_DIR}
)

target_link_libraries(ScriptVessel
	${ORBITER_LIB}
	${ORBITER_SDK_LIB}
	lua::lib
)

add_dependencies(ScriptVessel
	${OrbiterTgt}
	Orbitersdk
)

# Installation
if(WIN32) # Windows: upstream
install(TARGETS ScriptVessel
	RUNTIME
	DESTINATION ${ORBITER_INSTALL_MODULE_DIR}
)
else() # Linux port
install(TARGETS ScriptVessel
	LIBRARY
	DESTINATION ${ORBITER_INSTALL_MODULE_DIR}
)
endif() # WIN32

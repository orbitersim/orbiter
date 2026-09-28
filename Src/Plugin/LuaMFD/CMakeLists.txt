# Copyright (c) Martin Schweiger
# Licensed under the MIT License

if(WIN32) # Windows: upstream
add_library(LuaMFD SHARED
	LuaMFD.cpp
	LuaMFD.rc
	MfdInterpreter.cpp
)
else() # Linux port
add_library(LuaMFD SHARED
	LuaMFD.cpp
	MfdInterpreter.cpp
)
endif() # WIN32
if(NOT WIN32) # Linux port
orbiter_rc_resources(LuaMFD LuaMFD.rc)
endif() # NOT WIN32

target_include_directories(LuaMFD
	PUBLIC ${ORBITER_SOURCE_SDK_INCLUDE_DIR}
	PUBLIC ${MODULE_COMMON_DIR}
	PUBLIC ${ORBITER_SOURCE_ROOT_DIR}/Src/Module/LuaScript/LuaInterpreter
)

target_link_libraries(LuaMFD
	${ORBITER_LIB}
	${ORBITER_SDK_LIB}
	lua::lib
	LuaInterpreter
)

add_dependencies(LuaMFD
	${OrbiterTgt}
	Orbitersdk
	LuaInterpreter
)

set_target_properties(LuaMFD
	PROPERTIES
	FOLDER Modules
)

# Installation
if(WIN32) # Windows: upstream
install(TARGETS LuaMFD
	RUNTIME
	DESTINATION ${ORBITER_INSTALL_PLUGIN_DIR}
)
else() # Linux port
install(TARGETS LuaMFD
	LIBRARY
	DESTINATION ${ORBITER_INSTALL_PLUGIN_DIR}
)
endif() # WIN32

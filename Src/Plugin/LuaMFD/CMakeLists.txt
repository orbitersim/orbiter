# Copyright (c) Martin Schweiger
# Licensed under the MIT License

add_library(LuaMFD SHARED
	LuaMFD.cpp
	LuaMFD.rc
	MfdInterpreter.cpp
)

target_include_directories(LuaMFD
	PUBLIC ${ORBITER_SOURCE_SDK_INCLUDE_DIR}
	PUBLIC ${MODULE_COMMON_DIR}
	PUBLIC ${ORBITER_SOURCE_ROOT_DIR}/Src/Module/LuaScript/LuaInterpreter
)

target_link_libraries(LuaMFD
	${ORBITER_LIB}
	${ORBITER_SDK_LIB}
	lua::lib
	${LUAINTERPRETER_LIB}
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
install(TARGETS LuaMFD
	RUNTIME
	DESTINATION ${ORBITER_INSTALL_PLUGIN_DIR}
)

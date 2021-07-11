add_library(LuaConsole SHARED
	LuaConsole.cpp
	LuaConsole.rc
	ConsoleInterpreter.cpp
	ConsoleCfg.cpp
)

target_include_directories(LuaConsole
	PUBLIC ${ORBITER_SOURCE_SDK_INCLUDE_DIR}
	PUBLIC ${MODULE_COMMON_DIR}
)

target_link_libraries(LuaConsole
	${ORBITER_LIB}
	${ORBITER_SDK_LIB}
	${LUA_LIB}
	${LUAINTERPRETER_LIB}
)

add_dependencies(LuaConsole
	Orbiter
	Orbitersdk
	LuaInterpreter
)

set_target_properties(LuaConsole
	PROPERTIES
	FOLDER Modules
)

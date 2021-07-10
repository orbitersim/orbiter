set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${ORBITER_BINARY_ROOT_DIR})

add_library(LuaInterpreter SHARED
	Interpreter.cpp
	lua_vessel_mtd.cpp
)

target_include_directories(LuaInterpreter
	PUBLIC ${ORBITER_SOURCE_SDK_INCLUDE_DIR}
)

target_link_libraries(LuaInterpreter
	${ORBITER_LIB}
	${ORBITER_SDK_LIB}
	${LUA_LIB}
)

add_dependencies(LuaInterpreter
	Orbiter
	Orbitersdk
)

set_target_properties(LuaInterpreter
	PROPERTIES
	FOLDER Modules/Lua
)

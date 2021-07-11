add_library(LuaMFD SHARED
	LuaMFD.cpp
	LuaMFD.rc
	MfdInterpreter.cpp
)

target_include_directories(LuaMFD
	PUBLIC ${ORBITER_SOURCE_SDK_INCLUDE_DIR}
	PUBLIC ${MODULE_COMMON_DIR}
)

target_link_libraries(LuaMFD
	${ORBITER_LIB}
	${ORBITER_SDK_LIB}
	${LUA_LIB}
	${LUAINTERPRETER_LIB}
)

add_dependencies(LuaMFD
	Orbiter
	Orbitersdk
	LuaInterpreter
)

set_target_properties(LuaMFD
	PROPERTIES
	FOLDER Modules
)

# Copyright (c) Martin Schweiger
# Licensed under the MIT License

# Core library to go to the Orbiter main directory
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${ORBITER_BINARY_ROOT_DIR})

add_library(LuaInline SHARED
	LuaInline.cpp
)

target_include_directories(LuaInline
	PUBLIC ${ORBITER_SOURCE_SDK_INCLUDE_DIR}
	PUBLIC ${ORBITER_SOURCE_ROOT_DIR}/Src/Module/LuaScript/LuaInterpreter
)

target_link_libraries(LuaInline
	${ORBITER_LIB}
	${ORBITER_SDK_LIB}
	LuaInterpreter
	lua::lib
)

add_dependencies(LuaInline
	${OrbiterTgt}
	Orbitersdk
	LuaInterpreter
)

set_target_properties(LuaInline
	PROPERTIES
	FOLDER Modules/Lua
)

# Installation
install(TARGETS LuaInline
	RUNTIME
	DESTINATION ${ORBITER_INSTALL_ROOT_DIR}
)

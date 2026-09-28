# Copyright (c) Martin Schweiger
# Licensed under the MIT License

# Core library to go to the Orbiter main directory
if(WIN32) # Windows: upstream
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${ORBITER_BINARY_ROOT_DIR})
else() # Linux port
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${ORBITER_BINARY_ROOT_DIR})
endif() # WIN32

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
if(WIN32) # Windows: upstream
install(TARGETS LuaInline
	RUNTIME
	DESTINATION ${ORBITER_INSTALL_ROOT_DIR}
)
else() # Linux port
install(TARGETS LuaInline
	LIBRARY
	DESTINATION ${ORBITER_INSTALL_ROOT_DIR}
)
endif() # WIN32

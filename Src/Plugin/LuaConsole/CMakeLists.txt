# Copyright (c) Martin Schweiger
# Licensed under the MIT License

add_library(LuaConsole SHARED
	LuaConsole.cpp
	ConsoleInterpreter.cpp
)

target_include_directories(LuaConsole
	PUBLIC ${ORBITER_SOURCE_SDK_INCLUDE_DIR}
	PUBLIC ${ORBITER_SOURCE_ROOT_DIR}/Src/Module/LuaScript/LuaInterpreter
)

target_link_libraries(LuaConsole
	${ORBITER_LIB}
	${ORBITER_SDK_LIB}
	lua::lib
	LuaInterpreter
	imgui
)

add_dependencies(LuaConsole
	${OrbiterTgt}
	Orbitersdk
	LuaInterpreter
)

set_target_properties(LuaConsole
	PROPERTIES
	FOLDER Modules
)

# Installation
if(WIN32) # Windows: upstream
install(TARGETS LuaConsole
	RUNTIME
	DESTINATION ${ORBITER_INSTALL_PLUGIN_DIR}
)
else() # Linux port
install(TARGETS LuaConsole
	LIBRARY
	DESTINATION ${ORBITER_INSTALL_PLUGIN_DIR}
)
endif() # WIN32

install(FILES LuaConsole.png
	DESTINATION ${ORBITER_INSTALL_TEXTURES_DIR}/MenuInfoBar
)
if(NOT WIN32) # Linux port
# not upstream: also in the build tree, so Orbiter run from there finds it
file(COPY LuaConsole.png DESTINATION ${ORBITER_BINARY_TEXTURES_DIR}/MenuInfoBar)
endif() # NOT WIN32

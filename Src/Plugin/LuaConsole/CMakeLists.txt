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
	${LUAINTERPRETER_LIB}
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
install(TARGETS LuaConsole
	RUNTIME
	DESTINATION ${ORBITER_INSTALL_PLUGIN_DIR}
)

install(FILES LuaConsole.png
	DESTINATION ${ORBITER_INSTALL_TEXTURES_DIR}/MenuInfoBar
)

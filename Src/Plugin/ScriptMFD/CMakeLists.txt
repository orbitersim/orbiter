# Copyright (c) Martin Schweiger
# Licensed under the MIT License

add_library(ScriptMFD SHARED
	ScriptMFD.cpp
	ScriptMFD.rc
)

target_include_directories(ScriptMFD
	PUBLIC ${ORBITER_SOURCE_SDK_INCLUDE_DIR}
	PUBLIC ${MODULE_COMMON_DIR}
)

target_link_libraries(ScriptMFD
	${ORBITER_LIB}
	${ORBITER_SDK_LIB}
	${LUA_LIB}
	${LUAINTERPRETER_LIB}
)

add_dependencies(ScriptMFD
	${OrbiterTgt}
	Orbitersdk
)

set_target_properties(ScriptMFD
	PROPERTIES
	FOLDER Modules
)

# Installation
install(TARGETS ScriptMFD
	RUNTIME
	DESTINATION ${ORBITER_INSTALL_PLUGIN_DIR}
)

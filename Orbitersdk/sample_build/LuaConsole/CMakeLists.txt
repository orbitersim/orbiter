# Licensed under the MIT License


# =============================================================================
# LuaConsole
# =============================================================================

set(PRJ LuaConsole)

set(Sources
	ConsoleCfg.cpp
	ConsoleInterpreter.cpp
	LuaConsole.cpp
)

set(Headers
	ConsoleCfg.cpp
	ConsoleInterpreter.cpp
	LuaConsole.cpp
	resource.h
)
set(Other
	LuaConsole.rc
)
	
source_group(APIHeaders FILES ${APIHeaders})
source_group(Headers FILES ${Headers})
source_group(Sources FILES ${Sources})
source_group(Other FILES ${Other})

add_library(${PRJ} SHARED
	${Sources}
	${Headers}
    ${APIHeaders}
	${Other}
)

target_link_libraries(${PRJ} debug ${DebugLib})
target_link_libraries(${PRJ} optimized ${ReleaseLib})

set_target_properties(${PRJ}
	PROPERTIES
	RUNTIME_OUTPUT_DIRECTORY_DEBUG ${PLUGIN_DIR}
	RUNTIME_OUTPUT_DIRECTORY_RELEASE ${PLUGIN_DIR}
	FOLDER Misc
)

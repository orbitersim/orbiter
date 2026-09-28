# Licensed under the MIT License


# =============================================================================
# LuaConsole
# =============================================================================

set(PRJ LuaConsole)

if(WIN32) # Windows: upstream
set(Sources
	ConsoleCfg.cpp
	ConsoleInterpreter.cpp
	LuaConsole.cpp
)
else() # Linux port
set(Sources
	# ConsoleCfg.cpp left out: not installed, LuaConsole no longer has it
	ConsoleInterpreter.cpp
	LuaConsole.cpp
)
endif() # WIN32

if(WIN32) # Windows: upstream
set(Headers
	ConsoleCfg.cpp
	ConsoleInterpreter.cpp
	LuaConsole.cpp
	resource.h
)
else() # Linux port
set(Headers
	# ConsoleCfg.cpp left out: not installed, LuaConsole no longer has it
	ConsoleInterpreter.cpp
	LuaConsole.cpp
	# resource.h left out: not installed, LuaConsole no longer has resources
)
endif() # WIN32
if(WIN32) # Windows: upstream
set(Other
	LuaConsole.rc
)
else() # Linux port
set(Other
	# LuaConsole.rc left out: not installed, LuaConsole no longer has resources
)
endif() # WIN32
	
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

if(WIN32) # Windows: upstream
set_target_properties(${PRJ}
	PROPERTIES
	RUNTIME_OUTPUT_DIRECTORY_DEBUG ${PLUGIN_DIR}
	RUNTIME_OUTPUT_DIRECTORY_RELEASE ${PLUGIN_DIR}
	FOLDER Misc
)
else() # Linux port
set_target_properties(${PRJ}
	PROPERTIES
	LIBRARY_OUTPUT_DIRECTORY_DEBUG ${PLUGIN_DIR}
	LIBRARY_OUTPUT_DIRECTORY_RELEASE ${PLUGIN_DIR}
	FOLDER Misc
)
endif() # WIN32

# Licensed under the MIT License


# =============================================================================
# LuaMFD
# =============================================================================

set(PRJ LuaMFD)

set(Sources
	LuaMFD.cpp
	MfdInterpreter.cpp
)

set(Headers
	LuaMFD.h
	MfdInterpreter.h
	resource.h
)
set(Other
	LuaMFD.rc
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
	FOLDER MFD
)

# Licensed under the MIT License


# =============================================================================
# DialogTemplate
# =============================================================================

set(PRJ DialogTemplate)

set(Sources
	DialogTemplate.cpp
)
set(Headers
	resource.h
)
if(WIN32) # Windows: upstream
set(Other
	DialogTemplate.rc
)
else() # Linux port
set(Other
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
if(NOT WIN32) # Linux port
orbiter_rc_resources(${PRJ} DialogTemplate.rc)
endif() # NOT WIN32

target_link_libraries(${PRJ} debug ${DebugLib})
target_link_libraries(${PRJ} optimized ${ReleaseLib})

if(WIN32) # Windows: upstream
set_target_properties(${PRJ}
	PROPERTIES
	RUNTIME_OUTPUT_DIRECTORY_DEBUG ${MODULE_DIR}
	RUNTIME_OUTPUT_DIRECTORY_RELEASE ${MODULE_DIR}
	FOLDER Template
)
else() # Linux port
set_target_properties(${PRJ}
	PROPERTIES
	LIBRARY_OUTPUT_DIRECTORY_DEBUG ${MODULE_DIR}
	LIBRARY_OUTPUT_DIRECTORY_RELEASE ${MODULE_DIR}
	FOLDER Template
)
endif() # WIN32

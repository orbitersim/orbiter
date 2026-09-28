# Licensed under the MIT License

# =============================================================================
# DGConfigurator
# =============================================================================

set(PRJ DGConfigurator)

set(Sources
	DGConfigurator.cpp
)

set(Headers
	DGC_resource.h
)
if(WIN32) # Windows: upstream
set(Other
	DGConfigurator.rc
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
orbiter_rc_resources(${PRJ} DGConfigurator.rc)
endif() # NOT WIN32

target_link_libraries(${PRJ} debug ${DebugLib})
target_link_libraries(${PRJ} optimized ${ReleaseLib})

if(WIN32) # Windows: upstream
set_target_properties(${PRJ}
	PROPERTIES
	RUNTIME_OUTPUT_DIRECTORY_DEBUG ${STARTUP_DIR}
	RUNTIME_OUTPUT_DIRECTORY_RELEASE ${STARTUP_DIR}
	FOLDER DeltaGlider
)
else() # Linux port
set_target_properties(${PRJ}
	PROPERTIES
	LIBRARY_OUTPUT_DIRECTORY_DEBUG ${STARTUP_DIR}
	LIBRARY_OUTPUT_DIRECTORY_RELEASE ${STARTUP_DIR}
	FOLDER DeltaGlider
)
endif() # WIN32



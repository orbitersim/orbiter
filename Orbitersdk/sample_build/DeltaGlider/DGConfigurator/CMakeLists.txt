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
set(Other
	DGConfigurator.rc
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
	RUNTIME_OUTPUT_DIRECTORY_DEBUG ${STARTUP_DIR}
	RUNTIME_OUTPUT_DIRECTORY_RELEASE ${STARTUP_DIR}
	FOLDER DeltaGlider
)



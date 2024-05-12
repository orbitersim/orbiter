# Licensed under the MIT License

# =============================================================================
# LaunchpadParamTemplate
# =============================================================================

set(PRJ LaunchpadParamTemplate)

set(Sources
	LParam.cpp
)
set(Headers
	resource.h
)
set(Other
	LParam.rc
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
	RUNTIME_OUTPUT_DIRECTORY_DEBUG ${MODULE_DIR}
	RUNTIME_OUTPUT_DIRECTORY_RELEASE ${MODULE_DIR}
	FOLDER Template
)

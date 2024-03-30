# Licensed under the MIT License


# =============================================================================
# AtlantisConfig
# =============================================================================

set(PRJ AtlantisConfig)

set(Sources
	../Common.cpp
	AtlantisConfig.cpp
)
set(Headers
	AC_resource.h
)
set(Other
	AConfigurator.rc
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

#target_include_directories(${PRJ} PUBLIC ${INCLUDE_DIR} ${IncludeDirs})

set_target_properties(${PRJ}
	PROPERTIES
	RUNTIME_OUTPUT_DIRECTORY_DEBUG ${STARTUP_DIR}
	RUNTIME_OUTPUT_DIRECTORY_RELEASE ${STARTUP_DIR}
	FOLDER Atlantis
)

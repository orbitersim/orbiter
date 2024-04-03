# Licensed under the MIT License

# =============================================================================
# Atlantis_Tank
# =============================================================================

set(PRJ Atlantis_Tank)

set(Sources
	../Common.cpp
	Atlantis_Tank.cpp
)
	
source_group(APIHeaders FILES ${APIHeaders})
source_group(Sources FILES ${Sources})

add_library(${PRJ} SHARED
	${Sources}
    ${APIHeaders}
)

add_dependencies(${PRJ}
	Atlantis_SRB
)

target_link_libraries(${PRJ} debug ${DebugLib} Atlantis_SRB)
target_link_libraries(${PRJ} optimized ${ReleaseLib} Atlantis_SRB)

#target_include_directories(${PRJ} PUBLIC ${INCLUDE_DIR} ${IncludeDirs})

set_target_properties(${PRJ}
	PROPERTIES
	RUNTIME_OUTPUT_DIRECTORY_DEBUG ${MODULE_DIR}
	RUNTIME_OUTPUT_DIRECTORY_RELEASE ${MODULE_DIR}
	FOLDER Atlantis
)

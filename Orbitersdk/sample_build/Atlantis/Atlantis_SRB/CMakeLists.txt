# Licensed under the MIT License

# =============================================================================
# Atlantis_SRB
# =============================================================================

set(PRJ Atlantis_SRB)

set(Sources
	../Common.cpp
	Atlantis_SRB.cpp
)
	
source_group(APIHeaders FILES ${APIHeaders})
source_group(Sources FILES ${Sources})

add_library(${PRJ} SHARED
	${Sources}
    ${APIHeaders}
)

target_link_libraries(${PRJ} debug ${DebugLib})
target_link_libraries(${PRJ} optimized ${ReleaseLib})

#target_include_directories(${PRJ} PUBLIC ${INCLUDE_DIR} ${IncludeDirs})

set_target_properties(${PRJ}
	PROPERTIES
	RUNTIME_OUTPUT_DIRECTORY_DEBUG ${MODULE_DIR}
	RUNTIME_OUTPUT_DIRECTORY_RELEASE ${MODULE_DIR}
	FOLDER Atlantis
)
# Licensed under the MIT License

# =============================================================================
# Solarsail
# =============================================================================

set(PRJ Solarsail)
set(MESH_DIR ${ROOT_DIR}/Meshes)

set(Sources
	Solarsail.cpp
	SailLua.cpp
)

set(Headers
	Solarsail.h
)

set(Other
	${MESH_DIR}/Solarsail.msh
)


source_group(APIHeaders FILES ${APIHeaders})
source_group(Headers FILES ${Headers})
source_group(Sources FILES ${Sources})
source_group(Other FILES ${Other})

add_library(${PRJ} SHARED
	${Sources}
	${Headers}
	${Other}
    ${APIHeaders}
)

# Execute Mesh Compiler ------------------------------

# scan mesh files for labels and output to header file
add_custom_command(OUTPUT meshres.h
	COMMAND ${ORBITER_TOOL_MESHC} /I ${MESH_DIR}/SolarSail.msh /P - /O meshres.h
	DEPENDS ${MESH_DIR}/SolarSail.msh
)
add_custom_target(Solarsail_Prerequisites
	DEPENDS meshres.h
)

# -----------------------------------------------------

add_dependencies(${PRJ}
	Solarsail_Prerequisites
)

target_link_libraries(${PRJ} debug ${DebugLib})
target_link_libraries(${PRJ} optimized ${ReleaseLib})

set_target_properties(${PRJ}
	PROPERTIES
	RUNTIME_OUTPUT_DIRECTORY_DEBUG ${MODULE_DIR}
	RUNTIME_OUTPUT_DIRECTORY_RELEASE ${MODULE_DIR}
	FOLDER Vessel
)
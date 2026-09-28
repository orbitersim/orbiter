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

if(WIN32) # Windows: upstream
set(Headers
	Solarsail.h
)
else() # Linux port
set(Headers
	SolarSail.h # not upstream: the case of the installed file (Linux names are case sensitive)
)
endif() # WIN32

if(WIN32) # Windows: upstream
set(Other
	${MESH_DIR}/Solarsail.msh
)
else() # Linux port
set(Other
	${MESH_DIR}/SolarSail.msh # not upstream: the case of the installed file
)
endif() # WIN32


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

if(WIN32) # Windows: upstream
set_target_properties(${PRJ}
	PROPERTIES
	RUNTIME_OUTPUT_DIRECTORY_DEBUG ${MODULE_DIR}
	RUNTIME_OUTPUT_DIRECTORY_RELEASE ${MODULE_DIR}
	FOLDER Vessel
)
else() # Linux port
set_target_properties(${PRJ}
	PROPERTIES
	LIBRARY_OUTPUT_DIRECTORY_DEBUG ${MODULE_DIR}
	LIBRARY_OUTPUT_DIRECTORY_RELEASE ${MODULE_DIR}
	FOLDER Vessel
)
endif() # WIN32

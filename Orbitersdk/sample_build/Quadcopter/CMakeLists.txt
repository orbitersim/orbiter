# Licensed under the MIT License


# =============================================================================
# Quadcopter
# =============================================================================

set(PRJ Quadcopter)
set(MESH_DIR ${ROOT_DIR}/Meshes)

set(Sources
	Quadcopter.cpp
	QuadcopterLua.cpp
	QuadcopterSubsys.cpp
	PropulsionSubsys.cpp
	${SAMPLES_SRC_DIR}/Common/Vessel/Instrument.cpp
)

set(Headers
	Quadcopter.h
	QuadcopterLua.h
	QuadcopterSubsys.h
	PropulsionSubsys.h
	${SAMPLES_SRC_DIR}/Common/Vessel/Instrument.h
)
set(Other
	${MESH_DIR}/quadcopter.msh
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
	RUNTIME_OUTPUT_DIRECTORY_DEBUG ${MODULE_DIR}
	RUNTIME_OUTPUT_DIRECTORY_RELEASE ${MODULE_DIR}
	FOLDER Vessel
)

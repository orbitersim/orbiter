# Licensed under the MIT License


# =============================================================================
# Atlantis
# =============================================================================

set(PRJ Atlantis)
set(MESH_DIR ${ROOT_DIR}/Meshes/Atlantis)

set(Sources
	AscentAP.cpp
	Atlantis.cpp
	PlBayOp.cpp
	../Common.cpp
	${SAMPLES_SRC_DIR}/Common/Dialog/Graph.cpp
	${SAMPLES_SRC_DIR}/Common/Dialog/TabDlg.cpp
)

set(Headers
	AscentAP.h
	Atlantis.h
	PlBayOp.h
	resource.h
	${SAMPLES_SRC_DIR}/Common/Dialog/Graph.h
	${SAMPLES_SRC_DIR}/Common/Dialog/TabDlg.h
)
set(Other
	Atlantis.rc
	down.ico
	ico00001.ico
	ico00002.ico
	ico00003.ico
	ico00004.ico
	icon1.ico
	${MESH_DIR}/Atlantis.msh
	${MESH_DIR}/AtlantisVC.msh
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


# Execute Mesh Compiler ------------------------------

# scan mesh files for labels and output to header file
add_custom_command(OUTPUT meshres.h
	COMMAND ${ORBITER_TOOL_MESHC} /I ${MESH_DIR}/Atlantis.msh /P - /O meshres.h
	DEPENDS ${MESH_DIR}/Atlantis.msh
)
add_custom_command(OUTPUT meshres_vc.h
	COMMAND ${ORBITER_TOOL_MESHC} /I ${MESH_DIR}/AtlantisVC.msh /P _VC /O meshres_vc.h
	DEPENDS ${MESH_DIR}/AtlantisVC.msh
)
add_custom_target(Atlantis_Prerequisites
	DEPENDS meshres.h meshres_vc.h
)
# -----------------------------------------------------


add_dependencies(${PRJ}
	Atlantis_Prerequisites
	Atlantis_SRB
	Atlantis_Tank
)

target_link_libraries(${PRJ} debug ${DebugLib} Atlantis_Tank)
target_link_libraries(${PRJ} optimized ${ReleaseLib} Atlantis_Tank)

set_target_properties(${PRJ}
	PROPERTIES
	RUNTIME_OUTPUT_DIRECTORY_DEBUG ${MODULE_DIR}
	RUNTIME_OUTPUT_DIRECTORY_RELEASE ${MODULE_DIR}
	FOLDER Atlantis
)

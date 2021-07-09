set(MESH_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Meshes)
set(TEXTURE_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Textures)
set(CONFIG_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Config)

add_library(Atlantis SHARED
	AscentAP.cpp
	Atlantis.cpp
	PlBayOp.cpp
	Atlantis.rc
	../Common.cpp
	${ORBITER_SOURCE_MODULE_DIR}/Common/Dialog/Graph.cpp
	${ORBITER_SOURCE_MODULE_DIR}/Common/Dialog/TabDlg.cpp
)

target_include_directories(Atlantis
	PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}
	PUBLIC ${ORBITER_SOURCE_SDK_INCLUDE_DIR}
	PUBLIC ${ORBITER_SOURCE_MODULE_DIR}
	PUBLIC ${CMAKE_CURRENT_BINARY_DIR}                 # for the dynamically generated header files
)

target_link_libraries(Atlantis
	${ORBITER_LIB}
	${ORBITER_SDK_LIB}
	${ORBITER_DLGCTRL_LIB}
	UxTheme.lib
#	$<TARGET_LINKER_FILE:Atlantis_Tank>
)

# scan mesh file for labels and output to header file
add_custom_target(ATLANTIS_MESHRES_H
	COMMAND ${ORBITER_TOOL_MESHC} /I ${MESH_SOURCE_DIR}/Atlantis.msh /P - /O meshres.h
	BYPRODUCTS meshres.h
)
add_dependencies(ATLANTIS_MESHRES_H
	meshc
)

# scan mesh file for labels and output to header file
add_custom_target(ATLANTIS_MESHRES_VC_H
	COMMAND ${ORBITER_TOOL_MESHC} /I ${MESH_SOURCE_DIR}/AtlantisVC.msh /P _VC /O meshres_vc.h
	BYPRODUCTS meshres_vc.h
)
add_dependencies(ATLANTIS_MESHRES_VC_H
	meshc
)

add_custom_command(
	TARGET Atlantis PRE_BUILD
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${CONFIG_SOURCE_DIR}/ ${CMAKE_BINARY_DIR}/Config/Vessels
)

add_custom_command(
	TARGET Atlantis PRE_BUILD
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${MESH_SOURCE_DIR}/ ${CMAKE_BINARY_DIR}/Meshes/Atlantis
)

add_custom_command(
	TARGET Atlantis PRE_BUILD
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${TEXTURE_SOURCE_DIR}/ ${ORBITER_BINARY_TEXTURES_DIR}/Atlantis
)

add_dependencies(Atlantis
	Orbiter
	Orbitersdk
	DlgCtrl
	Atlantis_SRB
	Atlantis_Tank
	ATLANTIS_MESHRES_H
	ATLANTIS_MESHRES_VC_H
)


# Copyright (c) Martin Schweiger
# Licensed under the MIT License

set(VESSEL_NAME Atlantis)
set(INSTALL_SUBDIR Atlantis)

set(MESH_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Meshes)
set(TEXTURE_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Textures)
set(TEXTURE2_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Textures2)
set(CONFIG_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Config)
set(THUMBNAIL_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Thumbnail)

add_library(${VESSEL_NAME} SHARED
	AscentAP.cpp
	Atlantis.cpp
	PlBayOp.cpp
	../Common.cpp
)

set_target_properties(${VESSEL_NAME}
	PROPERTIES
	FOLDER Vessels/${VESSEL_NAME}
)

target_include_directories(${VESSEL_NAME}
	PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}
	PUBLIC ${ORBITER_SOURCE_SDK_INCLUDE_DIR}
	PUBLIC ${ORBITER_SOURCE_MODULE_DIR}
	PUBLIC ${CMAKE_CURRENT_BINARY_DIR}                 # for the dynamically generated header files
)

target_link_libraries(${VESSEL_NAME}
	${ORBITER_LIB}
	${ORBITER_SDK_LIB}
	${ORBITER_DLGCTRL_LIB}
	UxTheme.lib
	$<TARGET_LINKER_FILE:Atlantis_Tank>
	imgui
)

# scan mesh files for labels and output to header file
add_custom_command(OUTPUT meshres.h
	COMMAND ${ORBITER_TOOL_MESHC} /I ${MESH_SOURCE_DIR}/Atlantis.msh /P - /O meshres.h
	DEPENDS ${MESH_SOURCE_DIR}/Atlantis.msh
)
add_custom_command(OUTPUT meshres_vc.h
	COMMAND ${ORBITER_TOOL_MESHC} /I ${MESH_SOURCE_DIR}/AtlantisVC.msh /P _VC /O meshres_vc.h
	DEPENDS ${MESH_SOURCE_DIR}/AtlantisVC.msh
)
add_custom_target(${VESSEL_NAME}_Prerequisites
	DEPENDS meshres.h meshres_vc.h
)
add_dependencies(${VESSEL_NAME}_Prerequisites
	meshc
)

add_custom_command(
	TARGET ${VESSEL_NAME} PRE_BUILD
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${CONFIG_SOURCE_DIR}/ ${CMAKE_BINARY_DIR}/Config/Vessels
)

add_custom_command(
	TARGET ${VESSEL_NAME} PRE_BUILD
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${MESH_SOURCE_DIR}/ ${ORBITER_BINARY_MESH_DIR}/${INSTALL_SUBDIR}
)

add_custom_command(
	TARGET ${VESSEL_NAME} PRE_BUILD
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${TEXTURE_SOURCE_DIR}/ ${ORBITER_BINARY_TEXTURES_DIR}/${INSTALL_SUBDIR}
)

add_custom_command(
	TARGET ${VESSEL_NAME} PRE_BUILD
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${TEXTURE2_SOURCE_DIR}/ ${ORBITER_BINARY_TEXTURES2_DIR}/${INSTALL_SUBDIR}
)

add_custom_command(
	TARGET ${VESSEL_NAME} PRE_BUILD
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${THUMBNAIL_SOURCE_DIR}/ ${CMAKE_BINARY_DIR}/Images/Vessels/Default
)

add_dependencies(${VESSEL_NAME}
	${OrbiterTgt}
	Orbitersdk
	Atlantis_SRB
	Atlantis_Tank
	${VESSEL_NAME}_Prerequisites
)

set_target_properties(${VESSEL_NAME}_Prerequisites
	PROPERTIES
	FOLDER Vessels/${VESSEL_NAME}
)

# Installation
install(TARGETS ${VESSEL_NAME}
	RUNTIME
	DESTINATION ${ORBITER_INSTALL_MODULE_DIR}
)
install(DIRECTORY ${CONFIG_SOURCE_DIR}/
	DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/Config/Vessels
)
install(DIRECTORY ${MESH_SOURCE_DIR}/
	DESTINATION ${ORBITER_INSTALL_MESH_DIR}/${INSTALL_SUBDIR}
)
install(DIRECTORY ${TEXTURE_SOURCE_DIR}/
	DESTINATION ${ORBITER_INSTALL_TEXTURES_DIR}/${INSTALL_SUBDIR}
)
install(DIRECTORY ${TEXTURE2_SOURCE_DIR}/
	DESTINATION ${ORBITER_INSTALL_TEXTURES2_DIR}/${INSTALL_SUBDIR}
)
install(DIRECTORY ${THUMBNAIL_SOURCE_DIR}/
	DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/Images/Vessels/Default
)

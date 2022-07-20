# Copyright (c) Martin Schweiger
# Licensed under the MIT License

set(VESSEL_NAME ISS)

set(MESH_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Meshes)
set(TEXTURE_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Textures)
set(CONFIG_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Config)
set(THUMBNAIL_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Thumbnail)

srcdir_to_tgtlist("${CMAKE_CURRENT_SOURCE_DIR}/Config" "${ORBITER_BINARY_CONFIG_DIR}/Vessels" srclist tgtlist)
set(cfg_tgtlist ${tgtlist})
add_custom_command(
	OUTPUT ${tgtlist}
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${CONFIG_SOURCE_DIR}/ ${ORBITER_BINARY_CONFIG_DIR}/Vessels
	DEPENDS ${srclist}
)

srcdir_to_tgtlist("${MESH_SOURCE_DIR}" "${ORBITER_BINARY_MESH_DIR}/${VESSEL_NAME}" srclist tgtlist)
set(msh_tgtlist ${tgtlist})
add_custom_command(
	OUTPUT ${tgtlist}
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${MESH_SOURCE_DIR}/ ${ORBITER_BINARY_MESH_DIR}/${VESSEL_NAME}
	DEPENDS ${srclist}
)

srcdir_to_tgtlist("${TEXTURE_SOURCE_DIR}" "${ORBITER_BINARY_TEXTURES_DIR}/${VESSEL_NAME}" srclist tgtlist)
set(tex_tgtlist ${tgtlist})
add_custom_command(
	OUTPUT ${tgtlist}
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${TEXTURE_SOURCE_DIR}/ ${ORBITER_BINARY_TEXTURES_DIR}/${VESSEL_NAME}
	DEPENDS ${srclist}
)

srcdir_to_tgtlist("${THUMBNAIL_SOURCE_DIR}" "${CMAKE_BINARY_DIR}/Images/Vessels/Default" srclist tgtlist)
set(thmb_tgtlist ${tgtlist})
add_custom_command(
	OUTPUT ${tgtlist}
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${THUMBNAIL_SOURCE_DIR}/ ${CMAKE_BINARY_DIR}/Images/Vessels/Default
	DEPENDS ${srclist}
)

add_custom_target(${VESSEL_NAME}
	DEPENDS ${cfg_tgtlist} ${msh_tgtlist} ${tex_tgtlist} ${thmb_tgtlist}
)

set_target_properties(${VESSEL_NAME}
	PROPERTIES
	FOLDER Vessels
)

add_dependencies(${OrbiterTgt}
	${VESSEL_NAME}
)

# Installation
install(DIRECTORY ${CONFIG_SOURCE_DIR}/
	DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/Config/Vessels
)
install(DIRECTORY ${MESH_SOURCE_DIR}/
	DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/Meshes/${VESSEL_NAME}
)
install(DIRECTORY ${TEXTURE_SOURCE_DIR}/
	DESTINATION ${ORBITER_INSTALL_TEXTURES_DIR}/${VESSEL_NAME}
)
install(DIRECTORY ${THUMBNAIL_SOURCE_DIR}/
	DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/Images/Vessels/Default
)

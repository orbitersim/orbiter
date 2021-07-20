set(MESH_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Meshes)
set(TEXTURE_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Textures)
set(CONFIG_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Config)

srcdir_to_tgtlist("${CMAKE_CURRENT_SOURCE_DIR}/Config" "${ORBITER_BINARY_CONFIG_DIR}/Vessels" srclist tgtlist)
set(cfg_tgtlist ${tgtlist})
add_custom_command(
	OUTPUT ${tgtlist}
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${CONFIG_SOURCE_DIR}/ ${ORBITER_BINARY_CONFIG_DIR}/Vessels
	DEPENDS ${srclist}
)

srcdir_to_tgtlist("${MESH_SOURCE_DIR}" "${ORBITER_BINARY_MESH_DIR}" srclist tgtlist)
set(msh_tgtlist ${tgtlist})
add_custom_command(
	OUTPUT ${tgtlist}
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${MESH_SOURCE_DIR}/ ${ORBITER_BINARY_MESH_DIR}
	DEPENDS ${srclist}
)

srcdir_to_tgtlist("${TEXTURE_SOURCE_DIR}" "${ORBITER_BINARY_TEXTURES_DIR}/ISS" srclist tgtlist)
set(tex_tgtlist ${tgtlist})
add_custom_command(
	OUTPUT ${tgtlist}
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${TEXTURE_SOURCE_DIR}/ ${ORBITER_BINARY_TEXTURES_DIR}/ISS
	DEPENDS ${srclist}
)

add_custom_target(ISS
	DEPENDS ${cfg_tgtlist} ${msh_tgtlist} ${tex_tgtlist}
)

set_target_properties(ISS
	PROPERTIES
	FOLDER Vessels
)

add_dependencies(Orbiter
	ISS
)

# Installation
install(DIRECTORY ${CONFIG_SOURCE_DIR}/
	DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/Config/Vessels
)
install(DIRECTORY ${MESH_SOURCE_DIR}/
	DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/Meshes
)
install(DIRECTORY ${TEXTURE_SOURCE_DIR}/
	DESTINATION ${ORBITER_INSTALL_TEXTURES_DIR}/ISS
)
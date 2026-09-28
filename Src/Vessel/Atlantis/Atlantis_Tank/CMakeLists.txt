# Copyright (c) Martin Schweiger
# Licensed under the MIT License

set(VESSEL_NAME Atlantis_Tank)
set(INSTALL_SUBDIR Atlantis)

set(MESH_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Meshes)
set(TEXTURE_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Textures)
set(CONFIG_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Config)
set(THUMBNAIL_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Thumbnail)

add_library(${VESSEL_NAME} SHARED
	Atlantis_Tank.cpp
)

set_target_properties(${VESSEL_NAME}
	PROPERTIES
	FOLDER Vessels/${VESSEL_NAME}
)

target_include_directories(${VESSEL_NAME}
	PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/../Atlantis
	PUBLIC ${ORBITER_SOURCE_SDK_INCLUDE_DIR}
)

if(WIN32) # Windows: upstream
target_link_libraries(${VESSEL_NAME}
	${ORBITER_LIB}
	${ORBITER_SDK_LIB}
	$<TARGET_LINKER_FILE:Atlantis_SRB>
)
else() # Linux port
target_link_libraries(${VESSEL_NAME}
	${ORBITER_LIB}
	${ORBITER_SDK_LIB}
	Atlantis_SRB # not upstream: by target name, CMake turns a path to a prefix-less .so into -l<name>
)
endif() # WIN32

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
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${THUMBNAIL_SOURCE_DIR}/ ${CMAKE_BINARY_DIR}/Images/Vessels/Default
)

add_dependencies(${VESSEL_NAME}
	${OrbiterTgt}
	Orbitersdk
	Atlantis_SRB
)

if(WIN32) # Windows: upstream
install(TARGETS ${VESSEL_NAME}
	RUNTIME
	DESTINATION ${ORBITER_INSTALL_MODULE_DIR}
)
else() # Linux port
install(TARGETS ${VESSEL_NAME}
	LIBRARY
	DESTINATION ${ORBITER_INSTALL_MODULE_DIR}
)
endif() # WIN32
install(DIRECTORY ${CONFIG_SOURCE_DIR}/
	DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/Config/Vessels
)
install(DIRECTORY ${MESH_SOURCE_DIR}/
	DESTINATION ${ORBITER_INSTALL_MESH_DIR}/${INSTALL_SUBDIR}
)
install(DIRECTORY ${TEXTURE_SOURCE_DIR}/
	DESTINATION ${ORBITER_INSTALL_TEXTURES_DIR}/${INSTALL_SUBDIR}
)
install(DIRECTORY ${THUMBNAIL_SOURCE_DIR}/
	DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/Images/Vessels/Default
)

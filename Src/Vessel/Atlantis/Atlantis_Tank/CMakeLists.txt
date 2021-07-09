set(MESH_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Meshes)
set(TEXTURE_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Textures)
set(CONFIG_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Config)

add_library(Atlantis_Tank SHARED
	Atlantis_Tank.cpp
)

target_include_directories(Atlantis_Tank
	PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/../Atlantis
	PUBLIC ${ORBITER_SOURCE_SDK_INCLUDE_DIR}
)

target_link_libraries(Atlantis_Tank
	${ORBITER_LIB}
	${ORBITER_SDK_LIB}
	$<TARGET_LINKER_FILE:Atlantis_SRB>
)

add_custom_command(
	TARGET Atlantis_Tank PRE_BUILD
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${CONFIG_SOURCE_DIR}/ ${CMAKE_BINARY_DIR}/Config/Vessels
)

add_custom_command(
	TARGET Atlantis_Tank PRE_BUILD
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${MESH_SOURCE_DIR}/ ${CMAKE_BINARY_DIR}/Meshes/Atlantis
)

add_custom_command(
	TARGET Atlantis_Tank PRE_BUILD
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${TEXTURE_SOURCE_DIR}/ ${ORBITER_BINARY_TEXTURES_DIR}/Atlantis
)

add_dependencies(Atlantis_Tank
	Orbiter
	Orbitersdk
	Atlantis_SRB
)


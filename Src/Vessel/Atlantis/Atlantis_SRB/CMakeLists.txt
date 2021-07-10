set(MESH_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Meshes)
set(TEXTURE_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Textures)
set(CONFIG_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Config)

add_library(Atlantis_SRB SHARED
	Atlantis_SRB.cpp
	../Common.cpp
)

set_target_properties(Atlantis_SRB
	PROPERTIES
	FOLDER Vessels/Atlantis_SRB
)

target_include_directories(Atlantis_SRB
	PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/../Atlantis
	PUBLIC ${ORBITER_SOURCE_SDK_INCLUDE_DIR}
	PUBLIC ${CMAKE_CURRENT_BINARY_DIR}                 # for the dynamically generated header files
)

target_link_libraries(Atlantis_SRB
	${ORBITER_LIB}
	${ORBITER_SDK_LIB}
)

# scan mesh file for labels and output to header file
add_custom_target(ATLANTIS_SRB_MESHRES_H
	COMMAND ${ORBITER_TOOL_MESHC} /I ${MESH_SOURCE_DIR}/Atlantis_SRB.msh /P - /O meshres.h
	BYPRODUCTS meshres.h
)
add_dependencies(ATLANTIS_SRB_MESHRES_H
	meshc
)

add_custom_command(
	TARGET Atlantis_SRB PRE_BUILD
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${CONFIG_SOURCE_DIR}/ ${CMAKE_BINARY_DIR}/Config/Vessels
)

add_custom_command(
	TARGET Atlantis_SRB PRE_BUILD
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${MESH_SOURCE_DIR}/ ${CMAKE_BINARY_DIR}/Meshes/Atlantis
)

add_custom_command(
	TARGET Atlantis_SRB PRE_BUILD
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${TEXTURE_SOURCE_DIR}/ ${ORBITER_BINARY_TEXTURES_DIR}/Atlantis
)

add_dependencies(Atlantis_SRB
	Orbiter
	Orbitersdk
	ATLANTIS_SRB_MESHRES_H
)

set_target_properties(ATLANTIS_SRB_MESHRES_H
	PROPERTIES
	FOLDER Vessels/Atlantis_SRB/Prerequisites
)

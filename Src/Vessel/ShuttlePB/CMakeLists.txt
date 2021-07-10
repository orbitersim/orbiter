set(MESH_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Meshes)
set(TEXTURE_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Textures)
set(CONFIG_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Config)

add_library(ShuttlePB SHARED
	ShuttlePB.cpp
)

set_target_properties(ShuttlePB
	PROPERTIES
	FOLDER Vessels
)

target_include_directories(ShuttlePB
	PUBLIC ${ORBITER_SOURCE_SDK_INCLUDE_DIR}
)

target_link_libraries(ShuttlePB
	${ORBITER_LIB}
	${ORBITER_SDK_LIB}
)

add_dependencies(ShuttlePB
	Orbiter
	Orbitersdk
)

add_custom_command(
	TARGET ShuttlePB PRE_BUILD
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${CONFIG_SOURCE_DIR}/ ${CMAKE_BINARY_DIR}/Config/Vessels
)

add_custom_command(
	TARGET ShuttlePB PRE_BUILD
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${MESH_SOURCE_DIR}/ ${CMAKE_BINARY_DIR}/Meshes
)

add_custom_command(
	TARGET ShuttlePB PRE_BUILD
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${TEXTURE_SOURCE_DIR}/ ${ORBITER_BINARY_TEXTURES_DIR}   # this should really go to its own subdirectory
)

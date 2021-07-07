# Compile in Lua interface for executing HST functions?
set(ADD_SCRIPT_SUPPORT 1)

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${ORBITER_BINARY_VESSEL_DIR})

set(MESH_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Meshes)
set(TEXTURE_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Textures)
set(CONFIG_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Config)

add_library(HST SHARED
	HST.cpp
	HST_Lua.cpp
)

target_include_directories(HST
	PUBLIC ${ORBITER_SOURCE_SDK_INCLUDE_DIR}
)

target_link_libraries(HST
	${ORBITER_LIB}
	${ORBITER_SDK_LIB}
)

add_dependencies(HST
	Orbiter
	Orbitersdk
)

if(ADD_SCRIPT_SUPPORT)
	add_definitions(-DSCRIPTSUPPORT)
	target_link_libraries(HST
	${LUA_LIB}
	)
endif()

add_custom_command(
	TARGET HST PRE_BUILD
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${CONFIG_SOURCE_DIR}/ ${CMAKE_BINARY_DIR}/Config/Vessels
)

add_custom_command(
	TARGET HST PRE_BUILD
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${MESH_SOURCE_DIR}/ ${CMAKE_BINARY_DIR}/Meshes
)

add_custom_command(
	TARGET HST PRE_BUILD
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${TEXTURE_SOURCE_DIR}/ ${CMAKE_BINARY_DIR}/Textures   # this should really go to its own subdirectory
)

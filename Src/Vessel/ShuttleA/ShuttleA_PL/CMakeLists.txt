# Copyright (c) Martin Schweiger
# Licensed under the MIT License

if(WIN32) # Windows: upstream
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${ORBITER_BINARY_VESSEL_DIR})
else() # Linux port
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${ORBITER_BINARY_VESSEL_DIR})
endif() # WIN32

set(MESH_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Meshes)
set(TEXTURE_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Textures)
set(CONFIG_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Config)

add_library(ShuttleA_PL SHARED
	ShuttleA_pl.cpp
)

set_target_properties(ShuttleA_PL
	PROPERTIES
	FOLDER Vessels/ShuttleA
)

target_include_directories(ShuttleA_PL
	PUBLIC ${ORBITER_SOURCE_SDK_INCLUDE_DIR}
)

target_link_libraries(ShuttleA_PL
	${ORBITER_LIB}
	${ORBITER_SDK_LIB}
)

add_dependencies(ShuttleA_PL
	${OrbiterTgt}
	Orbitersdk
)

add_custom_command(
	TARGET ShuttleA_PL PRE_BUILD
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${CONFIG_SOURCE_DIR}/ ${CMAKE_BINARY_DIR}/Config/Vessels
)

add_custom_command(
	TARGET ShuttleA_PL PRE_BUILD
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${MESH_SOURCE_DIR}/ ${CMAKE_BINARY_DIR}/Meshes/ShuttleA
)

add_custom_command(
	TARGET ShuttleA_PL PRE_BUILD
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${TEXTURE_SOURCE_DIR}/ ${ORBITER_BINARY_TEXTURES_DIR}/ShuttleA
)

# Installation
if(WIN32) # Windows: upstream
install(TARGETS ShuttleA_PL
	RUNTIME
	DESTINATION ${ORBITER_INSTALL_MODULE_DIR}
)
else() # Linux port
install(TARGETS ShuttleA_PL
	LIBRARY
	DESTINATION ${ORBITER_INSTALL_MODULE_DIR}
)
endif() # WIN32
install(DIRECTORY ${CONFIG_SOURCE_DIR}/
	DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/Config/Vessels
)
install(DIRECTORY ${MESH_SOURCE_DIR}/
	DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/Meshes/ShuttleA
)
install(DIRECTORY ${TEXTURE_SOURCE_DIR}/
	DESTINATION ${ORBITER_INSTALL_TEXTURES_DIR}/ShuttleA
)

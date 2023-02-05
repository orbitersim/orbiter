# Copyright (c) Martin Schweiger
# Licensed under the MIT License

set(MESH_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Meshes)
#set(TEXTURE_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Textures)
set(CONFIG_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Config)

add_library(Quadcopter SHARED
	Quadcopter.cpp
	QuadcopterLua.cpp
	QuadcopterSubsys.cpp
	PropulsionSubsys.cpp
	${VESSEL_COMMON_DIR}/Instrument.cpp
)

set_target_properties(Quadcopter
	PROPERTIES
	FOLDER Vessels
)

target_include_directories(Quadcopter
	PUBLIC ${ORBITER_SOURCE_SDK_INCLUDE_DIR}
	PUBLIC ${VESSEL_COMMON_DIR}
)

target_link_libraries(Quadcopter
	${ORBITER_LIB}
	${ORBITER_SDK_LIB}
	lua::lib
)

add_dependencies(Quadcopter
	${OrbiterTgt}
	Orbitersdk
)

add_custom_command(
	TARGET Quadcopter PRE_BUILD
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${CONFIG_SOURCE_DIR}/ ${CMAKE_BINARY_DIR}/Config/Vessels
)

add_custom_command(
	TARGET Quadcopter PRE_BUILD
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${MESH_SOURCE_DIR}/ ${CMAKE_BINARY_DIR}/Meshes
)

#add_custom_command(
#	TARGET Quadcopter PRE_BUILD
#	COMMAND ${CMAKE_COMMAND} -E copy_directory ${TEXTURE_SOURCE_DIR}/ ${ORBITER_BINARY_TEXTURES_DIR}   # this should really go to its own subdirectory
#)

# Installation
install(TARGETS Quadcopter
	RUNTIME
	DESTINATION ${ORBITER_INSTALL_MODULE_DIR}
)
install(DIRECTORY ${CONFIG_SOURCE_DIR}/
	DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/Config/Vessels
)
install(DIRECTORY ${MESH_SOURCE_DIR}/
	DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/Meshes
)
#install(DIRECTORY ${TEXTURE_SOURCE_DIR}/
#	DESTINATION ${ORBITER_INSTALL_TEXTURES_DIR}
#)

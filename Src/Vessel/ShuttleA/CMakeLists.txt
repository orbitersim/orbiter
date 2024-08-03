# Copyright (c) Martin Schweiger
# Licensed under the MIT License

set(MESH_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Meshes)
set(TEXTURE_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Textures)
set(CONFIG_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Config)
set(SCRIPT_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Script)
set(THUMBNAIL_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Thumbnail)

add_library(ShuttleA SHARED
	adiball.cpp
	adictrl.cpp
	airlockswitch.cpp
	attref.cpp
	auxpodctrl.cpp
	dockcvrswitch.cpp
	gearswitch.cpp
	hudbutton.cpp
	InstrVs.cpp
	mfdbutton.cpp
	navbutton.cpp
	needlepair.cpp
	paneltext.cpp
	payloadctrl.cpp
	rcsswitch.cpp
	ShuttleA.cpp
	ShuttleALua.cpp
	sliderpair.cpp
	switches.cpp
	ShuttleA.rc
	${VESSEL_COMMON_DIR}/Instrument.cpp
)

set_target_properties(ShuttleA
	PROPERTIES
	FOLDER Vessels/ShuttleA
)

target_include_directories(ShuttleA
	PUBLIC ${ORBITER_SOURCE_SDK_INCLUDE_DIR}
	PUBLIC ${VESSEL_COMMON_DIR}
	PUBLIC ${ORBITER_SOURCE_MODULE_DIR}/ScnEditor      # for ScnEditorAPI.h
	PUBLIC ${CMAKE_CURRENT_BINARY_DIR}                 # for the dynamically generated header files
)

target_link_libraries(ShuttleA
	${ORBITER_LIB}
	${ORBITER_SDK_LIB}
	${ORBITER_DLGCTRL_LIB}
	lua::lib
)

# scan mesh files for labels and output to header file
add_custom_command(OUTPUT meshres.h
	COMMAND ${ORBITER_TOOL_MESHC} /I ${MESH_SOURCE_DIR}/ShuttleA.msh /P - /O meshres.h
	DEPENDS ${MESH_SOURCE_DIR}/ShuttleA.msh
)
add_custom_target(ShuttleA_Prerequisites
	DEPENDS meshres.h
)
add_dependencies(ShuttleA_Prerequisites
	meshc
)

add_custom_command(
	TARGET ShuttleA PRE_BUILD
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${CONFIG_SOURCE_DIR}/ ${CMAKE_BINARY_DIR}/Config/Vessels
)

add_custom_command(
	TARGET ShuttleA PRE_BUILD
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${MESH_SOURCE_DIR}/ ${CMAKE_BINARY_DIR}/Meshes/ShuttleA
)

add_custom_command(
	TARGET ShuttleA PRE_BUILD
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${TEXTURE_SOURCE_DIR}/ ${ORBITER_BINARY_TEXTURES_DIR}/ShuttleA
)

add_custom_command(
	TARGET ShuttleA PRE_BUILD
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${SCRIPT_SOURCE_DIR}/ ${CMAKE_BINARY_DIR}/Script/ShuttleA
)

add_custom_command(
	TARGET ShuttleA PRE_BUILD
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${THUMBNAIL_SOURCE_DIR}/ ${CMAKE_BINARY_DIR}/Images/Vessels/Default
)

add_dependencies(ShuttleA
	${OrbiterTgt}
	Orbitersdk
	DlgCtrl
	ShuttleA_Prerequisites
)

# ShuttleA payload class
add_subdirectory(ShuttleA_PL)

# Installation
install(TARGETS ShuttleA
	RUNTIME
	DESTINATION ${ORBITER_INSTALL_MODULE_DIR}
)
install(DIRECTORY ${CONFIG_SOURCE_DIR}/
	DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/Config/Vessels
)
install(DIRECTORY ${MESH_SOURCE_DIR}/
	DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/Meshes/ShuttleA
)
install(DIRECTORY ${TEXTURE_SOURCE_DIR}/
	DESTINATION ${ORBITER_INSTALL_TEXTURES_DIR}/ShuttleA
)
install(DIRECTORY ${SCRIPT_SOURCE_DIR}/
	DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/Script/ShuttleA
)
install(DIRECTORY ${THUMBNAIL_SOURCE_DIR}/
	DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/Images/Vessels/Default
)

if(ORBITER_MAKE_DOC)
	add_subdirectory(Doc)
	add_subdirectory(Help)
endif()

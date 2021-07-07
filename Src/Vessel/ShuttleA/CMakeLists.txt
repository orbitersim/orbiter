set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${ORBITER_BINARY_VESSEL_DIR})

set(MESH_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Meshes)
set(TEXTURE_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Textures)
set(CONFIG_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Config)

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

target_include_directories(ShuttleA
	PUBLIC ${ORBITER_SOURCE_SDK_INCLUDE_DIR}
	PUBLIC ${VESSEL_COMMON_DIR}
	PUBLIC ${ORBITER_SOURCE_MODULE_DIR}/ScnEditor      # for ScnEditorAPI.h
)

target_link_libraries(ShuttleA
	${ORBITER_LIB}
	${ORBITER_SDK_LIB}
	${ORBITER_DLGCTRL_LIB}
	${LUA_LIB}
)

add_dependencies(ShuttleA
	Orbiter
	Orbitersdk
	DlgCtrl
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
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${TEXTURE_SOURCE_DIR}/ ${CMAKE_BINARY_DIR}/Textures   # this should really go to its own subdirectory
)

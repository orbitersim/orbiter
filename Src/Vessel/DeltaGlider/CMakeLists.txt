# Copyright (c) Martin Schweiger
# Licensed under the MIT License

set(MESH_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Meshes)
set(TEXTURE_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Textures)
set(TEXTURE2_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Textures2)
set(CONFIG_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Config)
set(SCRIPT_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Script)
set(THUMBNAIL_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Thumbnail)

add_library(DeltaGlider SHARED
	DeltaGlider.cpp
	DGLua.cpp
	# Subsystems
	AAPSubsys.cpp
	AerodynSubsys.cpp
	AvionicsSubsys.cpp
	DGSubsys.cpp
	DockingSubsys.cpp
	FailureSubsys.cpp
	GearSubsys.cpp
	HoverSubsys.cpp
	HudCtrl.cpp
	LightSubsys.cpp
	MainRetroSubsys.cpp
	MfdSubsys.cpp
	PressureSubsys.cpp
	RcsSubsys.cpp
	ScramSubsys.cpp
	ThermalSubsys.cpp
	# Instruments
	DGSwitches.cpp
	FuelMfd.cpp
	Horizon.cpp
	InstrAoa.cpp
	InstrHsi.cpp
	${VESSEL_COMMON_DIR}/Instrument.cpp
	InstrVs.cpp
	MomentInd.cpp
	# Resources
	DeltaGlider.rc
)

set_target_properties(DeltaGlider
	PROPERTIES
	FOLDER Vessels/DeltaGlider
)

target_include_directories(DeltaGlider
	PUBLIC ${ORBITER_SOURCE_SDK_INCLUDE_DIR}
	PUBLIC ${VESSEL_COMMON_DIR}
	PUBLIC ${ORBITER_SOURCE_MODULE_DIR}/ScnEditor      # for ScnEditorAPI.h
	PUBLIC ${CMAKE_CURRENT_BINARY_DIR}                 # for the dynamically generated header files
)

target_link_libraries(DeltaGlider
	${ORBITER_LIB}
	${ORBITER_SDK_LIB}
	${ORBITER_DLGCTRL_LIB}
	lua::lib
	imgui
)

# scan mesh files for labels and output to header file
add_custom_command(OUTPUT meshres.h
	COMMAND ${ORBITER_TOOL_MESHC} /I ${MESH_SOURCE_DIR}/deltaglider.msh /P - /O meshres.h
	DEPENDS ${MESH_SOURCE_DIR}/deltaglider.msh
)
add_custom_command(OUTPUT meshres_vc.h
	COMMAND ${ORBITER_TOOL_MESHC} /I ${MESH_SOURCE_DIR}/deltaglider_vc.msh /P _VC /O meshres_vc.h
	DEPENDS ${MESH_SOURCE_DIR}/deltaglider_vc.msh
)
add_custom_command(OUTPUT meshres_p0.h
	COMMAND ${ORBITER_TOOL_MESHC} /I ${MESH_SOURCE_DIR}/dg_2dpanel0.msh /P _P0 /O meshres_p0.h
	DEPENDS ${MESH_SOURCE_DIR}/dg_2dpanel0.msh
)
add_custom_command(OUTPUT meshres_p1.h
	COMMAND ${ORBITER_TOOL_MESHC} /I ${MESH_SOURCE_DIR}/dg_2dpanel1.msh /P _P1 /O meshres_p1.h
	DEPENDS ${MESH_SOURCE_DIR}/dg_2dpanel1.msh
)
add_custom_target(DeltaGlider_Prerequisites
	DEPENDS meshres.h meshres_vc.h meshres_p0.h meshres_p1.h
)
add_dependencies(DeltaGlider_Prerequisites
	meshc
)

add_custom_command(
	TARGET DeltaGlider PRE_BUILD
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${CONFIG_SOURCE_DIR}/ ${CMAKE_BINARY_DIR}/Config/Vessels
)

add_custom_command(
	TARGET DeltaGlider PRE_BUILD
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${MESH_SOURCE_DIR}/ ${CMAKE_BINARY_DIR}/Meshes/DG
)

add_custom_command(
	TARGET DeltaGlider PRE_BUILD
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${TEXTURE_SOURCE_DIR}/ ${ORBITER_BINARY_TEXTURES_DIR}/DG
)

add_custom_command(
	TARGET DeltaGlider PRE_BUILD
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${TEXTURE2_SOURCE_DIR}/ ${ORBITER_BINARY_TEXTURES2_DIR}/DG
)

add_custom_command(
	TARGET DeltaGlider PRE_BUILD
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${SCRIPT_SOURCE_DIR}/ ${CMAKE_BINARY_DIR}/Script/DG
)

add_custom_command(
	TARGET DeltaGlider PRE_BUILD
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${THUMBNAIL_SOURCE_DIR}/ ${CMAKE_BINARY_DIR}/Images/Vessels/Default
)

add_dependencies(DeltaGlider
	${OrbiterTgt}
	Orbitersdk
	DlgCtrl
	DeltaGlider_Prerequisites
)

set_target_properties(DeltaGlider_Prerequisites
	PROPERTIES
	FOLDER Vessels/DeltaGlider
)

# Installation
install(TARGETS DeltaGlider
	RUNTIME
	DESTINATION ${ORBITER_INSTALL_MODULE_DIR}
)
install(DIRECTORY ${CONFIG_SOURCE_DIR}/
	DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/Config/Vessels
)
install(DIRECTORY ${MESH_SOURCE_DIR}/
	DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/Meshes/DG
)
install(DIRECTORY ${TEXTURE_SOURCE_DIR}/
	DESTINATION ${ORBITER_INSTALL_TEXTURES_DIR}/DG
)
install(DIRECTORY ${TEXTURE2_SOURCE_DIR}/
	DESTINATION ${ORBITER_INSTALL_TEXTURES2_DIR}/DG
)
install(DIRECTORY ${SCRIPT_SOURCE_DIR}/
	DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/Script/DG
)
install(DIRECTORY ${THUMBNAIL_SOURCE_DIR}/
	DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/Images/Vessels/Default
)

add_subdirectory(DGConfigurator)
if(ORBITER_MAKE_DOC)
	add_subdirectory(Help)
endif()

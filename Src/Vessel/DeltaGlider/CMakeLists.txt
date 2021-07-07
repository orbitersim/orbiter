set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${ORBITER_BINARY_VESSEL_DIR})

set(MESH_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Meshes)
set(TEXTURE_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Textures)
set(CONFIG_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Config)
set(SCRIPT_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Script)

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
	${LUA_LIB}
)

# scan mesh file for labels and output to header file
add_custom_target(MESHRES_H
	COMMAND ${ORBITER_TOOL_MESHC} /I ${MESH_SOURCE_DIR}/deltaglider.msh /P - /O meshres.h
	BYPRODUCTS meshres.h
)
add_dependencies(MESHRES_H
	meshc
)

# scan mesh file for labels and output to header file
add_custom_target(MESHRES_VC_H
	COMMAND ${ORBITER_TOOL_MESHC} /I ${MESH_SOURCE_DIR}/deltaglider_vc.msh /P _VC /O meshres_vc.h
	BYPRODUCTS meshres_vc.h
)
add_dependencies(MESHRES_VC_H
	meshc
)

# scan mesh file for labels and output to header file
add_custom_target(MESHRES_P0_H
	COMMAND ${ORBITER_TOOL_MESHC} /I ${MESH_SOURCE_DIR}/dg_2dpanel0.msh /P _P0 /O meshres_p0.h
	BYPRODUCTS meshres_p0.h
)
add_dependencies(MESHRES_P0_H
	meshc
)

# scan mesh file for labels and output to header file
add_custom_target(MESHRES_P1_H
	COMMAND ${ORBITER_TOOL_MESHC} /I ${MESH_SOURCE_DIR}/dg_2dpanel1.msh /P _P1 /O meshres_p1.h
	BYPRODUCTS meshres_p1.h
)
add_dependencies(MESHRES_P1_H
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
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${TEXTURE_SOURCE_DIR}/ ${CMAKE_BINARY_DIR}/Textures/DG
)

add_custom_command(
	TARGET DeltaGlider PRE_BUILD
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${SCRIPT_SOURCE_DIR}/ ${CMAKE_BINARY_DIR}/Script/DG
)

add_dependencies(DeltaGlider
	Orbiter
	Orbitersdk
	DlgCtrl
	MESHRES_H
	MESHRES_VC_H
	MESHRES_P0_H
	MESHRES_P1_H
)


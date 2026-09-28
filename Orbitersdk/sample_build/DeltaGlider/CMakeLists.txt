# Licensed under the MIT License


# =============================================================================
# DeltaGlider
# =============================================================================

add_subdirectory(DGConfigurator)

set(PRJ DeltaGlider)
set(MESH_DIR ${ROOT_DIR}/Meshes/DG)

if(WIN32) # Windows: upstream
set(Sources
	DeltaGlider.cpp
	DGLua.cpp
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
	DGSwitches.cpp
	FuelMfd.cpp
	Horizon.cpp
	InstrAoa.cpp
	InstrHsi.cpp
	${SAMPLES_SRC_DIR}/Common/Vessel/Instrument.cpp
	InstrVs.cpp
	MomentInd.cpp
	${SAMPLES_SRC_DIR}/Common/Dialog/Graph.cpp
	${SAMPLES_SRC_DIR}/Common/Dialog/TabDlg.cpp
)
else() # Linux port
set(Sources
	DeltaGlider.cpp
	DGLua.cpp
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
	DGSwitches.cpp
	FuelMfd.cpp
	Horizon.cpp
	InstrAoa.cpp
	InstrHsi.cpp
	${SAMPLES_SRC_DIR}/Common/Vessel/Instrument.cpp
	InstrVs.cpp
	MomentInd.cpp
	# Common/Dialog/Graph.cpp and TabDlg.cpp left out: not installed, the DeltaGlider no longer uses them
)
endif() # WIN32

if(WIN32) # Windows: upstream
set(Headers
	DeltaGlider.h
	DGLua.cpp
	AAPSubsys.h
	AerodynSubsys.h
	AvionicsSubsys.h
	dg_vc_anim.h
	DGSubsys.h
	DockingSubsys.h
	FailureSubsys.h
	GearSubsys.h
	HoverSubsys.h
	HudCtrl.h
	LightSubsys.h
	MainRetroSubsys.h
	MfdSubsys.h
	PressureSubsys.h
	RcsSubsys.h
	ScramSubsys.h
	ThermalSubsys.h
	DGSwitches.h
	FuelMfd.h
	Horizon.h
	InstrAoa.h
	InstrHsi.h
	${SAMPLES_SRC_DIR}/Common/Vessel/Instrument.h
	InstrVs.h
	MomentInd.h
	${SAMPLES_SRC_DIR}/Common/Dialog/Graph.h
	${SAMPLES_SRC_DIR}/Common/Dialog/TabDlg.h
	resource.h
)
else() # Linux port
set(Headers
	DeltaGlider.h
	DGLua.cpp
	AAPSubsys.h
	AerodynSubsys.h
	AvionicsSubsys.h
	dg_vc_anim.h
	DGSubsys.h
	DockingSubsys.h
	FailureSubsys.h
	GearSubsys.h
	HoverSubsys.h
	HudCtrl.h
	LightSubsys.h
	MainRetroSubsys.h
	MfdSubsys.h
	PressureSubsys.h
	RcsSubsys.h
	ScramSubsys.h
	ThermalSubsys.h
	DGSwitches.h
	FuelMfd.h
	Horizon.h
	InstrAoa.h
	InstrHsi.h
	${SAMPLES_SRC_DIR}/Common/Vessel/Instrument.h
	InstrVs.h
	MomentInd.h
	# Common/Dialog/Graph.h and TabDlg.h left out: not installed, the DeltaGlider no longer uses them
	resource.h
)
endif() # WIN32
if(WIN32) # Windows: upstream
set(Other
	DeltaGlider.rc
	${MESH_DIR}/deltaglider.msh
	${MESH_DIR}/deltaglider_vc.msh
	${MESH_DIR}/dg_2dpanel0.msh
	${MESH_DIR}/dg_2dpanel1.msh
)
else() # Linux port
set(Other
	${MESH_DIR}/deltaglider.msh
	${MESH_DIR}/deltaglider_vc.msh
	${MESH_DIR}/dg_2dpanel0.msh
	${MESH_DIR}/dg_2dpanel1.msh
)
endif() # WIN32
	
source_group(APIHeaders FILES ${APIHeaders})
source_group(Headers FILES ${Headers})
source_group(Sources FILES ${Sources})
source_group(Other FILES ${Other})

add_library(${PRJ} SHARED
	${Sources}
	${Headers}
    ${APIHeaders}
	${Other}
)
if(NOT WIN32) # Linux port
orbiter_rc_resources(${PRJ} DeltaGlider.rc)
endif() # NOT WIN32


# Execute Mesh Compiler ------------------------------

# scan mesh files for labels and output to header file
add_custom_command(OUTPUT meshres.h
	COMMAND ${ORBITER_TOOL_MESHC} /I ${MESH_DIR}/deltaglider.msh /P - /O meshres.h
	DEPENDS ${MESH_DIR}/deltaglider.msh
)
add_custom_command(OUTPUT meshres_vc.h
	COMMAND ${ORBITER_TOOL_MESHC} /I ${MESH_DIR}/deltaglider_vc.msh /P _VC /O meshres_vc.h
	DEPENDS ${MESH_DIR}/deltaglider_vc.msh
)
add_custom_command(OUTPUT meshres_p0.h
	COMMAND ${ORBITER_TOOL_MESHC} /I ${MESH_DIR}/dg_2dpanel0.msh /P _P0 /O meshres_p0.h
	DEPENDS ${MESH_DIR}/dg_2dpanel0.msh
)
add_custom_command(OUTPUT meshres_p1.h
	COMMAND ${ORBITER_TOOL_MESHC} /I ${MESH_DIR}/dg_2dpanel1.msh /P _P1 /O meshres_p1.h
	DEPENDS ${MESH_DIR}/dg_2dpanel1.msh
)
add_custom_target(DeltaGlider_Prerequisites
	DEPENDS meshres.h meshres_vc.h meshres_p0.h meshres_p1.h
)
# -----------------------------------------------------


add_dependencies(${PRJ}
	DeltaGlider_Prerequisites
)

target_link_libraries(${PRJ} debug ${DebugLib})
target_link_libraries(${PRJ} optimized ${ReleaseLib})

if(WIN32) # Windows: upstream
set_target_properties(${PRJ}
	PROPERTIES
	RUNTIME_OUTPUT_DIRECTORY_DEBUG ${MODULE_DIR}
	RUNTIME_OUTPUT_DIRECTORY_RELEASE ${MODULE_DIR}
	FOLDER DeltaGlider
)
else() # Linux port
set_target_properties(${PRJ}
	PROPERTIES
	LIBRARY_OUTPUT_DIRECTORY_DEBUG ${MODULE_DIR}
	LIBRARY_OUTPUT_DIRECTORY_RELEASE ${MODULE_DIR}
	FOLDER DeltaGlider
)
endif() # WIN32



# Copyright (c) Martin Schweiger
# Licensed under the MIT License

# ==========================================================================
# SDK Sample Installation
# ==========================================================================

install(DIRECTORY 
	AscentMFD 
	Atlantis 
	DeltaGlider 
	DialogTemplate 
	DrawOrbits 
	DX9ExtMFD 
	GenericCamera 
	HST
	LaunchpadParamTemplate 
	MFDTemplate 
	Quadcopter
	LuaConsole
	LuaMFD
	ScriptMFD
	ScriptVessel
	Solarsail
	TransX
	DESTINATION ${ORBITER_INSTALL_SDK_DIR}/samples
)

install(FILES CMakeMain.txt DESTINATION ${ORBITER_INSTALL_ROOT_DIR} RENAME CMakeLists.txt)
if(NOT WIN32) # Linux port
# not upstream: the .rc compiler CMakeMain.txt includes (orbiter_rc_resources)
install(FILES ${ORBITER_SOURCE_ROOT_DIR}/cmake/rcresources.cmake ${ORBITER_SOURCE_ROOT_DIR}/cmake/rc2cpp.py DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/cmake)
endif() # NOT WIN32


	




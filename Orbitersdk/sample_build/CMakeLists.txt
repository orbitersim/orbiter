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


	




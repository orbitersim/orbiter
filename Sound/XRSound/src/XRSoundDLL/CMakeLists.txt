add_library(XRSound_dll SHARED
	DefaultSoundGroupPreSteps.cpp
	ModuleXRSoundEngine.cpp
	SoundPreSteps.cpp
	VesselXRSoundEngine.cpp
	XRSoundConfigFileParser.cpp
	XRSoundDLL.cpp
	XRSoundEngine.cpp
	XRSoundEngine30.cpp
	Resource.rc
	../Utils/ConfigFileParser.cpp
	../Utils/FileList.cpp
)

set_target_properties(XRSound_dll
	PROPERTIES
	RUNTIME_OUTPUT_DIRECTORY ${ORBITER_BINARY_PLUGIN_DIR}
	OUTPUT_NAME XRSound
	COMPILE_DEFINITIONS "XRSOUND_DLL_BUILD;_CRT_SECURE_NO_WARNINGS;_USRDLL;XRSOUNDDLL_EXPORTS"
)

target_include_directories(XRSound_dll PUBLIC
	${ORBITER_SOURCE_SDK_INCLUDE_DIR}
	${CMAKE_CURRENT_SOURCE_DIR}/../XRSoundLib
	${CMAKE_CURRENT_SOURCE_DIR}/../Utils
	${IRRKLANG_DIR}/include
)

target_link_libraries(XRSound_dll
	${ORBITER_LIB}
	${ORBITER_SDK_LIB}
	${IRRKLANG_LIB}
)

add_dependencies(XRSound_dll
	${OrbiterTgt}
	Orbitersdk
)

install(TARGETS XRSound_dll
	RUNTIME
	DESTINATION ${ORBITER_INSTALL_PLUGIN_DIR}
)

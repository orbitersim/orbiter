## XRSound_dll
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
	Utils/ConfigFileParser.cpp
	Utils/FileList.cpp
	ISound.cpp
)

set_target_properties(XRSound_dll PROPERTIES
	RUNTIME_OUTPUT_DIRECTORY ${ORBITER_BINARY_PLUGIN_DIR}
	OUTPUT_NAME XRSound
	COMPILE_DEFINITIONS "XRSOUND_DLL_BUILD;_CRT_SECURE_NO_WARNINGS;_USRDLL;XRSOUNDDLL_EXPORTS"
)

option(XRSOUND_DEBUG_GUI
	"Include the XRSound Audio debug window in Debug builds (needs Orbiter's ImGui dialogs, so OFF for an XRSound that loads in earlier Orbiter 2024 versions)"
	ON
)
# Debug builds only; the window never goes into a Release build.
if(XRSOUND_DEBUG_GUI)
	target_compile_definitions(XRSound_dll PRIVATE $<$<CONFIG:Debug>:XRSOUND_DEBUG_GUI>)
endif()

target_include_directories(XRSound_dll PUBLIC
	${ORBITER_SOURCE_SDK_INCLUDE_DIR}
	${CMAKE_CURRENT_SOURCE_DIR}/Utils
)

target_link_libraries(XRSound_dll
	${ORBITER_LIB}
	${ORBITER_SDK_LIB}
	imgui
)

add_dependencies(XRSound_dll
	${OrbiterTgt}
	Orbitersdk
	imgui
)

install(TARGETS XRSound_dll
	RUNTIME DESTINATION ${ORBITER_INSTALL_PLUGIN_DIR}
)

## XRSound_lib
add_library(XRSound_lib STATIC
	XRSound.cpp
	XRSoundImpl.cpp
)

set_target_properties(XRSound_lib
	PROPERTIES
	ARCHIVE_OUTPUT_DIRECTORY ${ORBITER_BINARY_SDK_DIR}/$<1:XRSound>
	OUTPUT_NAME XRSound
)

target_include_directories(XRSound_lib PUBLIC
	${ORBITER_SOURCE_SDK_INCLUDE_DIR}
	imgui
)

install(TARGETS XRSound_lib
	ARCHIVE DESTINATION ${ORBITER_INSTALL_SDK_DIR}/XRSound
)

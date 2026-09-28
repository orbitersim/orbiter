## XRSound_dll
if(WIN32) # Windows: upstream
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
)
else() # Linux port
add_library(XRSound_dll SHARED
	DefaultSoundGroupPreSteps.cpp
	ModuleXRSoundEngine.cpp
	SoundPreSteps.cpp
	VesselXRSoundEngine.cpp
	XRSoundConfigFileParser.cpp
	XRSoundDLL.cpp
	XRSoundEngine.cpp
	XRSoundEngine30.cpp
	Utils/ConfigFileParser.cpp
	Utils/FileList.cpp
	# not upstream: the PipeWire mixer and decoders that replace irrKlang
	Audio/AudioDecode.cpp
	Audio/AudioEngine.cpp
)
endif() # WIN32
if(NOT WIN32) # Linux port
orbiter_rc_resources(XRSound_dll Resource.rc)
endif() # NOT WIN32

if(WIN32) # Windows: upstream
set_target_properties(XRSound_dll PROPERTIES
	RUNTIME_OUTPUT_DIRECTORY ${ORBITER_BINARY_PLUGIN_DIR}
	OUTPUT_NAME XRSound
	COMPILE_DEFINITIONS "XRSOUND_DLL_BUILD;_CRT_SECURE_NO_WARNINGS;_USRDLL;XRSOUNDDLL_EXPORTS"
)
else() # Linux port
set_target_properties(XRSound_dll PROPERTIES
	LIBRARY_OUTPUT_DIRECTORY ${ORBITER_BINARY_PLUGIN_DIR}
	OUTPUT_NAME XRSound
	COMPILE_DEFINITIONS "XRSOUND_DLL_BUILD;_CRT_SECURE_NO_WARNINGS;_USRDLL;XRSOUNDDLL_EXPORTS"
)
endif() # WIN32

if(WIN32) # Windows: upstream
target_include_directories(XRSound_dll PUBLIC
	${ORBITER_SOURCE_SDK_INCLUDE_DIR}
	${CMAKE_CURRENT_SOURCE_DIR}/Utils
	${IRRKLANG_DIR}/include
)
else() # Linux port
target_include_directories(XRSound_dll PUBLIC
	${ORBITER_SOURCE_SDK_INCLUDE_DIR}
	${CMAKE_CURRENT_SOURCE_DIR}/Utils
	${AUDIO_DECODER_INCLUDE_DIRS}
)
endif() # WIN32

if(NOT WIN32) # Linux port
# PipeWire replaces irrKlang; Qt Widgets for ConfigFileParser's QMessageBox
endif() # NOT WIN32
if(WIN32) # Windows: upstream
target_link_libraries(XRSound_dll
	${ORBITER_LIB}
	${ORBITER_SDK_LIB}
	${IRRKLANG_LIB}
)
else() # Linux port
target_link_libraries(XRSound_dll
	${ORBITER_LIB}
	${ORBITER_SDK_LIB}
	PkgConfig::PIPEWIRE
	${AUDIO_DECODER_LIBS}
	Qt6::Widgets
)
endif() # WIN32

add_dependencies(XRSound_dll
	${OrbiterTgt}
	Orbitersdk
)

if(WIN32) # Windows: upstream
install(TARGETS XRSound_dll
	RUNTIME DESTINATION ${ORBITER_INSTALL_PLUGIN_DIR}
)
else() # Linux port
install(TARGETS XRSound_dll
	LIBRARY DESTINATION ${ORBITER_INSTALL_PLUGIN_DIR}
)
endif() # WIN32

## XRSound_lib
add_library(XRSound_lib STATIC
	XRSound.cpp
	XRSoundImpl.cpp
)

if(WIN32) # Windows: upstream
set_target_properties(XRSound_lib
	PROPERTIES
	ARCHIVE_OUTPUT_DIRECTORY ${ORBITER_BINARY_SDK_DIR}/$<1:XRSound>
	OUTPUT_NAME XRSound
)
else() # Linux port
set_target_properties(XRSound_lib
	PROPERTIES
	ARCHIVE_OUTPUT_DIRECTORY ${ORBITER_BINARY_SDK_DIR}/$<1:XRSound>
	OUTPUT_NAME XRSound
	POSITION_INDEPENDENT_CODE ON # linked into .so modules
)
endif() # WIN32

target_include_directories(XRSound_lib PUBLIC
	${ORBITER_SOURCE_SDK_INCLUDE_DIR}
)

install(TARGETS XRSound_lib
	ARCHIVE DESTINATION ${ORBITER_INSTALL_SDK_DIR}/XRSound
)

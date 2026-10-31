## XRSoundApiTest: the manual developer test for the XRSound API (see README.md).
## It is always built, so a change to the XRSound API that breaks it fails the build.  It is installed only with
## XRSOUND_INSTALL_API_TEST=ON, because it is not part of an Orbiter release.
add_library(XRSoundApiTest SHARED
	XRSoundApiTest.cpp
	XRSoundApiTest.h
	TestSupport.cpp
	LevelsAndSettings.cpp
	Playback.cpp
	BuiltInSounds.cpp
	Music.cpp
)

set_target_properties(XRSoundApiTest PROPERTIES
	RUNTIME_OUTPUT_DIRECTORY ${ORBITER_BINARY_PLUGIN_DIR}
	FOLDER Tests
)

# the test opens files with fopen, like the XRSound DLL itself
target_compile_definitions(XRSoundApiTest PRIVATE _CRT_SECURE_NO_WARNINGS)

target_include_directories(XRSoundApiTest PRIVATE
	${ORBITER_SOURCE_SDK_INCLUDE_DIR}
	${CMAKE_CURRENT_SOURCE_DIR}/../src
)

target_link_libraries(XRSoundApiTest
	XRSound_lib
	${ORBITER_LIB}
	${ORBITER_SDK_LIB}
)

add_dependencies(XRSoundApiTest
	${OrbiterTgt}
	Orbitersdk
)

option(XRSOUND_INSTALL_API_TEST
	"Install the XRSound API test plugin, its test tones, and its scenario (a manual developer test; see Sound/XRSound/tests/README.md)"
	OFF
)
if(XRSOUND_INSTALL_API_TEST)
	install(TARGETS XRSoundApiTest
		RUNTIME DESTINATION ${ORBITER_INSTALL_PLUGIN_DIR}
	)
	install(DIRECTORY Sounds/
		DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/XRSound/ApiTest
	)
	install(FILES "XRSound API Test.scn"
		DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/Scenarios/Tests/Manual
	)
endif()

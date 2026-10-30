Include(FetchContent)

FetchContent_Declare(
  Catch2
  GIT_REPOSITORY https://github.com/catchorg/Catch2.git
  GIT_TAG        v3.0.0-preview3
)

FetchContent_MakeAvailable(Catch2)

# Utility function
function(add_test_file test_name)
	add_executable(${test_name} "${test_name}.cpp")

	set_target_properties( ${test_name} PROPERTIES RUNTIME_OUTPUT_DIRECTORY "${ORBITER_BINARY_ROOT_DIR}" )

	target_include_directories(${test_name}
		PRIVATE ${ORBITER_SOURCE_SDK_INCLUDE_DIR}
		PRIVATE ${MODULE_COMMON_DIR}
		PRIVATE ${ORBITER_SOURCE_ROOT_DIR}/Src/Module/LuaScript/LuaInterpreter
	)

	target_link_libraries(${test_name}
		${ORBITER_LIB}
		${ORBITER_SDK_LIB}
		lua::lib
		LuaInterpreter
		Catch2::Catch2WithMain
	)

	add_dependencies(${test_name}
		${OrbiterTgt}
		Orbitersdk
		LuaInterpreter
	)

	add_test( 
		NAME ${test_name} 
		COMMAND $<TARGET_FILE:${test_name}>
		WORKING_DIRECTORY ${ORBITER_BINARY_ROOT_DIR}
	)
	set_tests_properties(${test_name} PROPERTIES TIMEOUT 30)
endfunction()

# Utility function for self-contained tests that only need a few source files
# (listed after the test name) and no Lua; it links only Catch2, so a test whose
# sources need more (e.g., the Orbiter import library) adds it after the call
function(add_core_test_file test_name)
	add_executable(${test_name} "${test_name}.cpp" ${ARGN})

	set_target_properties( ${test_name} PROPERTIES RUNTIME_OUTPUT_DIRECTORY "${ORBITER_BINARY_ROOT_DIR}" )

	target_include_directories(${test_name}
		PRIVATE ${ORBITER_SOURCE_SDK_INCLUDE_DIR}
		PRIVATE ${ORBITER_SOURCE_ROOT_DIR}/Src/Orbiter
	)

	target_link_libraries(${test_name} Catch2::Catch2WithMain)

	add_test(
		NAME ${test_name}
		COMMAND $<TARGET_FILE:${test_name}>
		WORKING_DIRECTORY ${ORBITER_BINARY_ROOT_DIR}
	)
	set_tests_properties(${test_name} PROPERTIES TIMEOUT 30)
endfunction()

# Register unit tests
add_test_file(Lua.Interpreter)
add_core_test_file(Collision.CollisionCore
	${ORBITER_SOURCE_ROOT_DIR}/Src/Orbiter/CollisionCore.cpp
	${ORBITER_SOURCE_ROOT_DIR}/Src/Orbiter/Vecmat.cpp
)

# Orbiter's log file writer, built from its own sources (Log.cpp refers to the TimeData global, so TimeData.cpp comes along)
add_core_test_file(Orbiter.Log
	${ORBITER_SOURCE_ROOT_DIR}/Src/Orbiter/Log.cpp
	${ORBITER_SOURCE_ROOT_DIR}/Src/Orbiter/TimeData.cpp
)
target_link_libraries(Orbiter.Log winmm version)   # timeGetTime for the log timestamps; file version info for PrintModules

# XRSound config file parser: needs no sound card and no running Orbiter, and never touches the audio engine
if(ORBITER_BUILD_XRSOUND)
	add_core_test_file(XRSound.Config
		${ORBITER_SOURCE_ROOT_DIR}/Sound/XRSound/src/XRSoundConfigFileParser.cpp
		${ORBITER_SOURCE_ROOT_DIR}/Sound/XRSound/src/Utils/ConfigFileParser.cpp
	)
	target_include_directories(XRSound.Config
		PRIVATE ${ORBITER_SOURCE_ROOT_DIR}/Sound/XRSound/src
		PRIVATE ${ORBITER_SOURCE_ROOT_DIR}/Sound/XRSound/src/Utils
	)
	# same definition the XRSound DLL itself is built with (the parser sources use fopen/sprintf)
	target_compile_definitions(XRSound.Config PRIVATE _CRT_SECURE_NO_WARNINGS)
	# XRSoundConfigFileParser::ParseVesselSoundConfig calls VESSEL::GetName / GetClassName, which Orbiter exports, so the
	# link needs the Orbiter import library (the tests themselves never call into Orbiter)
	target_link_libraries(XRSound.Config ${ORBITER_LIB})
	add_dependencies(XRSound.Config ${OrbiterTgt})
endif()

if (BUILD_ORBITER_SERVER)

	# Sanity check for scenario tests
	add_test(
		NAME "Scenario.SanityCheck"
		COMMAND $<TARGET_FILE:Orbiter_server> "--scenariox=${CMAKE_SOURCE_DIR}/Scenarios/Delta-glider/Smack!.scn" "--maxframes=60"
		WORKING_DIRECTORY ${ORBITER_BINARY_ROOT_DIR}
	)
	set_tests_properties(Scenario.SanityCheck PROPERTIES TIMEOUT 60)

	# Register scenario tests
	file(GLOB TestScenarios "${CMAKE_SOURCE_DIR}/Scenarios/Tests/*.scn")
	foreach(Scenario ${TestScenarios})
		get_filename_component(test_name ${Scenario} NAME_WE)
		add_test(
			NAME "Scenario.${test_name}"
			COMMAND $<TARGET_FILE:Orbiter_server> "--scenariox=${Scenario}"
			WORKING_DIRECTORY ${ORBITER_BINARY_ROOT_DIR}
		)
		set_tests_properties(Scenario.${test_name} PROPERTIES TIMEOUT 60)
	endforeach()

endif()

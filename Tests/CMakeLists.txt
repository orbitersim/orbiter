Include(FetchContent)

if(NOT WIN32) # Linux port
# v3.0.0-preview3 sizes a static array with MINSIGSTKSZ, which glibc 2.34+ turned into a sysconf() call
endif() # NOT WIN32
if(WIN32) # Windows: upstream
FetchContent_Declare(
  Catch2
  GIT_REPOSITORY https://github.com/catchorg/Catch2.git
  GIT_TAG        v3.0.0-preview3
)
else() # Linux port
FetchContent_Declare(
  Catch2
  GIT_REPOSITORY https://github.com/catchorg/Catch2.git
  GIT_TAG        v3.8.1
)
endif() # WIN32

if(NOT WIN32) # Linux port
# static Catch2: tracy.cmake turns BUILD_SHARED_LIBS on for everything after it
set(BUILD_SHARED_LIBS OFF)
endif() # NOT WIN32
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

if(NOT WIN32) # Linux port
# not upstream: unit tests for ported code that runs without the Orbiter exe; extra args are the port sources under test
function(add_unit_test test_name)
	add_executable(${test_name} "${test_name}.cpp" ${ARGN})

	set_target_properties( ${test_name} PROPERTIES RUNTIME_OUTPUT_DIRECTORY "${ORBITER_BINARY_ROOT_DIR}/Tests" )

	target_include_directories(${test_name}
		PRIVATE ${ORBITER_SOURCE_SDK_INCLUDE_DIR}
		PRIVATE ${ORBITER_SOURCE_DIR}
	)

	target_link_libraries(${test_name}
		Catch2::Catch2WithMain
	)

	add_test(
		NAME ${test_name}
		COMMAND $<TARGET_FILE:${test_name}>
		WORKING_DIRECTORY ${ORBITER_BINARY_ROOT_DIR}
	)
	set_tests_properties(${test_name} PROPERTIES TIMEOUT 30)
endfunction()

endif() # NOT WIN32
# Register unit tests
add_test_file(Lua.Interpreter)
if(NOT WIN32) # Linux port
# not upstream: LuaInterpreter.so imports the exe-side API (Orbiter.exe on Windows); functions bind lazily, the rest comes from test doubles
target_sources(Lua.Interpreter PRIVATE Lua.Interpreter.Exe.cpp)
set_target_properties(Lua.Interpreter PROPERTIES ENABLE_EXPORTS ON)
target_link_options(Lua.Interpreter PRIVATE -Wl,--unresolved-symbols=ignore-in-shared-libs)

# not upstream: proves the Catch2/ctest harness builds and runs
add_unit_test(Port.Harness)
# not upstream: Phase 1 core utilities
add_unit_test(Vecmat.Test ${ORBITER_SOURCE_DIR}/Vecmat.cpp)
add_unit_test(Astro.Test ${ORBITER_SOURCE_DIR}/Astro.cpp ${ORBITER_SOURCE_DIR}/TimeData.cpp)
add_unit_test(PathResolve.Test ${ORBITER_SOURCE_DIR}/PathResolve.cpp)
add_unit_test(Input.Test ${ORBITER_SOURCE_DIR}/Di7frame.cpp ${ORBITER_SOURCE_DIR}/Memstat.cpp)
# not upstream: Phase 2 Celbody modules, loaded like Orbiter loads them; the test exports the exe-side API they bind to
add_unit_test(Celbody.Test ${ORBITER_SOURCE_DIR}/PathResolve.cpp)
set_target_properties(Celbody.Test PROPERTIES ENABLE_EXPORTS ON)
add_dependencies(Celbody.Test Sol Sun Mercury Venus Earth Mars Jupiter Saturn Uranus Neptune Moon Io Europa Ganymede Callisto
	Mimas Enceladus Tethys Dione Rhea Titan Hyperion Iapetus EarthAtm2006 EarthAtmJ71G EarthAtmNRLMSISE00 MarsAtm2006 VenusAtm2006)
# not upstream: Phase 4 dialog builder on Orbiter.rc, rendered offscreen
add_unit_test(ResDialog.Test ${ORBITER_SOURCE_DIR}/ResDialog.cpp ${ORBITER_SOURCE_DIR}/ResDialog.h)
set_target_properties(ResDialog.Test PROPERTIES AUTOMOC ON)
target_link_libraries(ResDialog.Test Qt6::Widgets DlgCtrl)
orbiter_rc_resources(ResDialog.Test ${ORBITER_SOURCE_DIR}/Orbiter.rc EXE SYMBOL OrbiterResources)
orbiter_rc_resources(ResDialog.Test ${CMAKE_CURRENT_SOURCE_DIR}/ResMenu.Test.rc SYMBOL TestMenuResources)
set_tests_properties(ResDialog.Test PROPERTIES ENVIRONMENT QT_QPA_PLATFORM=offscreen)
endif() # NOT WIN32

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

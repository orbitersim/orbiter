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
		${LUA_LIB}
		${LUAINTERPRETER_LIB}
		Catch2::Catch2WithMain
	)

	add_dependencies(${test_name}
		Orbitersdk
		LuaInterpreter
	)

	if (BUILD_ORBITER_DX7)
		add_dependencies(${test_name} Orbiter)
	else()
		add_dependencies(${test_name} Orbiter_server)
	endif()
	
	add_test( 
		NAME ${test_name} 
		COMMAND $<TARGET_FILE:${test_name}>
		WORKING_DIRECTORY ${ORBITER_BINARY_ROOT_DIR}
	)
endfunction()

# Register unit tests
add_test_file(Lua.Interpreter)

# Register integration tests
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

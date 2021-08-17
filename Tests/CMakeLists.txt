# Google test disabled due to excessive configure time and runtime issues
# message(STATUS "Start configuring GoogleTest")
# 
#set(CMAKE_CXX_STANDARD 11)

#include(FetchContent)
#FetchContent_Declare(
#  GoogleTest
#  GIT_REPOSITORY https://github.com/google/googletest.git
#  GIT_TAG        release-1.11.0
#)

# For Windows: Prevent overriding the parent project's compiler/linker settings
#set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
#FetchContent_MakeAvailable(GoogleTest)
#include_directories(${gtest_SOURCE_DIR}/include ${gtest_SOURCE_DIR})

function(add_test_file test_name)
	add_executable(${test_name} "${test_name}.cc")

	set_target_properties( ${test_name} PROPERTIES RUNTIME_OUTPUT_DIRECTORY "${ORBITER_BINARY_ROOT_DIR}" )

	target_include_directories(${test_name}
		PUBLIC ${ORBITER_SOURCE_SDK_INCLUDE_DIR}
		PUBLIC ${MODULE_COMMON_DIR}
		PUBLIC ${ORBITER_SOURCE_ROOT_DIR}/Src/Module/LuaScript/LuaInterpreter
	)

	target_link_libraries(${test_name}
		${ORBITER_SDK_LIB}
		${LUA_LIB}
		${LUAINTERPRETER_LIB}
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

add_test_file(t_lua_interpreter)

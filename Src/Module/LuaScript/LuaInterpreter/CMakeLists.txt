# Copyright (c) Martin Schweiger
# Licensed under the MIT License

# Core library to go to the Orbiter main directory
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${ORBITER_BINARY_ROOT_DIR})
set(BUILD_OUT_DIR ${CMAKE_CURRENT_BINARY_DIR}/out)

add_library(LuaInterpreter SHARED
	Interpreter.cpp
	lua_vessel_mtd.cpp
)

target_include_directories(LuaInterpreter
	PUBLIC ${ORBITER_SOURCE_SDK_INCLUDE_DIR}
)

target_link_libraries(LuaInterpreter
	${ORBITER_LIB}
	${ORBITER_SDK_LIB}
	${LUA_LIB}
)

add_dependencies(LuaInterpreter
	Orbiter
	Orbitersdk
)

set_target_properties(LuaInterpreter
	PROPERTIES
	FOLDER Modules/Lua
)

if(ORBITER_MAKE_DOC)
	file(GLOB lua_in *.h *.cpp *.ld *.md *.lua)
	add_custom_command(
		OUTPUT ${BUILD_OUT_DIR}/index.html
		COMMAND ${CMAKE_COMMAND} -E copy ${lua_in} ${CMAKE_CURRENT_BINARY_DIR}
		COMMAND ${ldoc} .
		COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/interpreter.hhp ${BUILD_OUT_DIR}
		JOB_POOL htmlhelp
)
	add_custom_command(
		OUTPUT ${ORBITER_BINARY_SDK_DIR}/doc/orbiter_lua.chm
		DEPENDS ${BUILD_OUT_DIR}/index.html
		WORKING_DIRECTORY ${BUILD_OUT_DIR}
		COMMAND ${HTML_HELP_COMPILER} interpreter.hhp && (echo .) || (echo .)
		COMMAND ${CMAKE_COMMAND} -E copy_if_different ./interpreter.chm ${ORBITER_BINARY_SDK_DIR}/doc/orbiter_lua.chm
		JOB_POOL htmlhelp
)
	add_custom_target(orbiter_lua
		DEPENDS ${ORBITER_BINARY_SDK_DIR}/doc/orbiter_lua.chm
	)
	add_dependencies(Orbiter
		orbiter_lua
	)

	install(FILES ${ORBITER_BINARY_SDK_DIR}/doc/orbiter_lua.chm
		DESTINATION ${ORBITER_INSTALL_SDK_DIR}/doc
	)
endif()

# Installation
install(TARGETS LuaInterpreter
	RUNTIME
	DESTINATION ${ORBITER_INSTALL_ROOT_DIR}
)
install(FILES $<TARGET_LINKER_FILE:LuaInterpreter>
	DESTINATION ${ORBITER_INSTALL_SDK_DIR}/lib/Lua
)
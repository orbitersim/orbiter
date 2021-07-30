# Copyright (c) Martin Schweiger
# Licensed under the MIT License

cmake_minimum_required(VERSION 3.15)

project (plsplit VERSION 2021.1)

add_executable(plsplit
	plsplit.cpp
)

set_target_properties(plsplit
	PROPERTIES
	LINK_FLAGS "/SUBSYSTEM:CONSOLE"
	RUNTIME_OUTPUT_DIRECTORY_DEBUG ${CMAKE_CURRENT_BINARY_DIR}
	RUNTIME_OUTPUT_DIRECTORY_RELEASE ${CMAKE_CURRENT_BINARY_DIR}
)

add_custom_command(
	TARGET plsplit PRE_BUILD
	COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/DxTex.exe ${CMAKE_CURRENT_BINARY_DIR}
)

# Installation - this is currently skipped

install(TARGETS
	plsplit
	RUNTIME
	DESTINATION ${INSTALLDIR}
)
install(FILES DxTex.exe
	DESTINATION ${INSTALLDIR}
)

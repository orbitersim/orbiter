add_custom_command(
	OUTPUT CurrentState.htm CurrentState_img.htm
	COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/CurrentState.htm .
	COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/CurrentState_img.htm .
)
add_custom_target(CurrentState_htm
	DEPENDS CurrentState.htm CurrentState_img.htm
)
add_dependencies(Orbiter
	CurrentState_htm
)

install(FILES CurrentState.htm CurrentState_img.htm
	DESTINATION ${ORBITER_INSTALL_HTML_DIR}/Scenarios
)

add_subdirectory(Default)

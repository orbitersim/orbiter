add_custom_command(
	OUTPUT CurrentState.htm CurrentState_img.htm
	COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/CurrentState.htm .
	COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/CurrentState_img.htm .
)
add_custom_target(CurrentState_htm
	DEPENDS CurrentState.htm CurrentState_img.htm
)
add_dependencies(${OrbiterTgt}
	CurrentState_htm
)

install(FILES CurrentState.htm CurrentState_img.htm
	DESTINATION ${ORBITER_INSTALL_HTML_DIR}/Scenarios
)

if(WIN32) # Windows: upstream
if(ORBITER_MAKE_DOC)
	add_subdirectory(Default)
endif()
else() # Linux port
if(ORBITER_MAKE_DOC OR ORBITER_MAKE_HELP) # not upstream: ORBITER_MAKE_HELP
	add_subdirectory(Default)
endif()
endif() # WIN32

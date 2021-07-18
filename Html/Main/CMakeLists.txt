add_custom_target(Orbiter_chm ALL
	COMMAND ${HTML_HELP_COMPILER} ${CMAKE_CURRENT_SOURCE_DIR}/Orbiter.hhp
)

set(name Alps2016)
set(subdir Scenarios/Default)

file(GLOB ChmSources
	*.htm
	*.css
	*.jpg
	*.gif
	${name}.hhp
)

add_custom_command(OUTPUT ${ORBITER_BINARY_HTML_DIR}/${subdir}/${name}.chm
	COMMAND ${HHC_CMD} ${name}.hhp
	COMMAND ${CMAKE_COMMAND} -E rename ${name}.chm ${ORBITER_BINARY_HTML_DIR}/${subdir}/${name}.chm
	DEPENDS ${ChmSources}
	WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
	JOB_POOL htmlhelp
)

add_custom_target(${name}_chm
	DEPENDS ${ORBITER_BINARY_HTML_DIR}/${subdir}/${name}.chm
)
add_dependencies(${OrbiterTgt}
	${name}_chm
)

install(FILES ${ORBITER_BINARY_HTML_DIR}/${subdir}/${name}.chm
	DESTINATION ${ORBITER_INSTALL_HTML_DIR}/${subdir}
)

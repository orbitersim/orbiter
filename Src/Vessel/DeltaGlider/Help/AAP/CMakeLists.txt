set(name aap)
set(subdir Script/Stockvessels/DG)

file(GLOB ChmSources
	*.htm
	*.css
	*.jpg
	*.gif
	*.hhc
	*.hhk
	${name}.hhp
)

# copy everything to the build directory and compile there

add_custom_command(OUTPUT ${ORBITER_BINARY_HTML_DIR}/${subdir}/${name}.chm
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_SOURCE_DIR}/ ${CMAKE_CURRENT_BINARY_DIR}
	COMMAND ${HTML_HELP_COMPILER} ${name}.hhp && (echo .) || (echo .)
	COMMAND ${CMAKE_COMMAND} -E copy ${name}.chm ${ORBITER_BINARY_HTML_DIR}/${subdir}/
	DEPENDS ${ChmSources}
	JOB_POOL htmlhelp
)
add_custom_target(${name}_chm
	DEPENDS ${ORBITER_BINARY_HTML_DIR}/${subdir}/${name}.chm
)
add_dependencies(DeltaGlider
	${name}_chm
)

install(FILES ${ORBITER_BINARY_HTML_DIR}/${subdir}/${name}.chm
	DESTINATION ${ORBITER_INSTALL_HTML_DIR}/${subdir}
)

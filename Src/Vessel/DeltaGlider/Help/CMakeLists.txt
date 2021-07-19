set(name DeltaGlider)
set(subdir Vessels)

file(GLOB ChmSources
	*.htm
	*.css
	*.jpg
	*.gif
	*.hhc
	*.hhk
	${name}.hhp
)

set(dgsrc ${CMAKE_CURRENT_SOURCE_DIR}/..)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/config.ld.in ${CMAKE_CURRENT_BINARY_DIR}/config.ld)

# copy everything to the build directory and compile there

add_custom_command(OUTPUT ${name}.chm
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_SOURCE_DIR}/ ${CMAKE_CURRENT_BINARY_DIR}
	COMMAND ${CMAKE_COMMAND} -E copy ${ORBITER_SOURCE_HTML_DIR}/master.js ${CMAKE_CURRENT_BINARY_DIR}
	COMMAND ${CMAKE_COMMAND} -E copy ${ORBITER_SOURCE_HTML_DIR}/Orbiter.css ${CMAKE_CURRENT_BINARY_DIR}
	COMMAND ${ldoc} .
	COMMAND ${HTML_HELP_COMPILER} ${name}.hhp && (echo .) || (echo .)
	DEPENDS ${ChmSources}
)
add_custom_target(${name}_chm
	DEPENDS ${name}.chm
)
add_dependencies(DeltaGlider
	${name}_chm
)

install(FILES ${CMAKE_CURRENT_BINARY_DIR}/${name}.chm
	DESTINATION ${ORBITER_INSTALL_HTML_DIR}/${subdir}
)

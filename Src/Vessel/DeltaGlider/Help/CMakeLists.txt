set(name DeltaGlider)
set(name2 DG-S)
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

add_custom_command(OUTPUT ${ORBITER_BINARY_HTML_DIR}/Vessels/${name}.chm ${ORBITER_BINARY_HTML_DIR}/Vessels/${name2}.chm
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_SOURCE_DIR}/ ${CMAKE_CURRENT_BINARY_DIR}
	COMMAND ${CMAKE_COMMAND} -E copy ${ORBITER_SOURCE_HTML_DIR}/master.js ${CMAKE_CURRENT_BINARY_DIR}
	COMMAND ${CMAKE_COMMAND} -E copy ${ORBITER_SOURCE_HTML_DIR}/Orbiter.css ${CMAKE_CURRENT_BINARY_DIR}
	COMMAND ${ldoc} .
	COMMAND ${HTML_HELP_COMPILER} ${name}.hhp && (echo .) || (echo .)
	COMMAND ${HTML_HELP_COMPILER} ${name2}.hhp && (echo .) || (echo .)
	COMMAND ${CMAKE_COMMAND} -E copy ${name}.chm ${ORBITER_BINARY_HTML_DIR}/Vessels/
	COMMAND ${CMAKE_COMMAND} -E copy ${name2}.chm ${ORBITER_BINARY_HTML_DIR}/Vessels/
	DEPENDS ${ChmSources}
	JOB_POOL htmlhelp
)
add_custom_target(${name}_chm
	DEPENDS ${ORBITER_BINARY_HTML_DIR}/Vessels/${name}.chm ${ORBITER_BINARY_HTML_DIR}/Vessels/${name2}.chm
)
add_dependencies(DeltaGlider
	${name}_chm
)

install(FILES ${CMAKE_CURRENT_BINARY_DIR}/${name}.chm ${CMAKE_CURRENT_BINARY_DIR}/${name2}.chm
	DESTINATION ${ORBITER_INSTALL_HTML_DIR}/${subdir}
)

add_subdirectory(AAP)

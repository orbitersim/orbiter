set(name Atlantis)
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

# copy everything to the build directory and compile there

add_custom_command(OUTPUT ${ORBITER_BINARY_HTML_DIR}/Vessels/${name}.chm
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_SOURCE_DIR}/ ${CMAKE_CURRENT_BINARY_DIR}
	COMMAND ${CMAKE_COMMAND} -E copy ${ORBITER_SOURCE_HTML_DIR}/master.js ${CMAKE_CURRENT_BINARY_DIR}
	COMMAND ${CMAKE_COMMAND} -E copy ${ORBITER_SOURCE_HTML_DIR}/Orbiter.css ${CMAKE_CURRENT_BINARY_DIR}
	COMMAND ${HHC_CMD} ${name}.hhp
	COMMAND ${CMAKE_COMMAND} -E copy ${name}.chm ${ORBITER_BINARY_HTML_DIR}/Vessels/
	DEPENDS ${ChmSources}
	JOB_POOL htmlhelp
)
add_custom_target(${name}_chm
	DEPENDS ${ORBITER_BINARY_HTML_DIR}/Vessels/${name}.chm
)
add_dependencies(Atlantis
	${name}_chm
)

install(FILES ${CMAKE_CURRENT_BINARY_DIR}/${name}.chm
	DESTINATION ${ORBITER_INSTALL_HTML_DIR}/${subdir}
)

# Help documents ship as folders of HTML pages. A help file referenced in code
# or scenarios as "Html/<subdir>/<name>.chm" is opened from "Html/<subdir>/<name>/".
#
# orbiter_help_pages(<name> <subdir> [LDOC] [ALIASES <alias>...])
#   Copies the HTML project in the current source directory to the help folder.
#   LDOC runs ldoc in the copied folder first (needs config.ld there).
#   ALIASES publishes the same pages under additional help names.
function(orbiter_help_pages name subdir)
	cmake_parse_arguments(H "LDOC" "" "ALIASES" ${ARGN})
	set(work ${CMAKE_BINARY_DIR}/help_work/${subdir}/${name})
	set(stamp ${CMAKE_BINARY_DIR}/help_work/${subdir}/${name}.stamp)
	set(out ${ORBITER_BINARY_HTML_DIR}/${subdir}/${name})
	file(GLOB_RECURSE deps CONFIGURE_DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/*.htm ${CMAKE_CURRENT_SOURCE_DIR}/*.html
		${CMAKE_CURRENT_SOURCE_DIR}/*.css ${CMAKE_CURRENT_SOURCE_DIR}/*.jpg ${CMAKE_CURRENT_SOURCE_DIR}/*.gif
		${CMAKE_CURRENT_SOURCE_DIR}/*.png ${CMAKE_CURRENT_SOURCE_DIR}/*.hhc)
	set(cmds
		COMMAND ${CMAKE_COMMAND} -E make_directory ${work}
		COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_SOURCE_DIR}/ ${work}
		COMMAND ${CMAKE_COMMAND} -E copy ${ORBITER_SOURCE_HTML_DIR}/master.js ${ORBITER_SOURCE_HTML_DIR}/Orbiter.css ${work}
	)
	set(ldocdeps)
	if(H_LDOC)
		if(EXISTS ${CMAKE_CURRENT_BINARY_DIR}/config.ld)
			list(APPEND cmds COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_BINARY_DIR}/config.ld ${work})
		endif()
		list(APPEND cmds COMMAND ${CMAKE_COMMAND} -E chdir ${work} ${ldoc} .)
		set(ldocdeps lua-exe lfs CopyLDoc)
	endif()
	list(APPEND cmds
		COMMAND ${CMAKE_COMMAND} -E copy_directory ${work} ${out}
	)
	foreach(alias ${H_ALIASES})
		list(APPEND cmds COMMAND ${CMAKE_COMMAND} -E copy_directory ${work} ${ORBITER_BINARY_HTML_DIR}/${subdir}/${alias})
	endforeach()
	add_custom_command(OUTPUT ${stamp}
		${cmds}
		COMMAND ${CMAKE_COMMAND} -E touch ${stamp}
		DEPENDS ${deps} ${ldocdeps}
		WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
	)
	add_custom_target(help_${name} ALL DEPENDS ${stamp})
	set_target_properties(help_${name} PROPERTIES FOLDER Help)
	install(DIRECTORY ${out} DESTINATION ${ORBITER_INSTALL_HTML_DIR}/${subdir})
endfunction()

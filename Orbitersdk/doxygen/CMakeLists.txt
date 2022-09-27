configure_file(${CMAKE_CURRENT_SOURCE_DIR}/Doxyfile.in ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile
)

set(out ${ORBITER_BINARY_SDK_DIR}/doc/API_Reference.chm)

file(GLOB_RECURSE doxysrc
	${ORBITER_SOURCE_SDK_DIR}/include/*
)

set(api_ref_out "")
add_custom_command(
	OUTPUT ${out}
	DEPENDS ${doxysrc}
	COMMAND ${DOXYGEN_EXECUTABLE} Doxyfile 2> nul
	COMMAND ${CMAKE_COMMAND} -E copy_if_different html/API_Reference.chm ${out}
	JOB_POOL htmlhelp
)
list(APPEND api_ref_out ${out})

if(LATEX_FOUND)
	add_custom_command(
		OUTPUT ${ORBITER_BINARY_SDK_DIR}/doc/API_Reference.pdf
		DEPENDS ${out}
		COMMAND ${PDFTEX_CMD} refman.tex
		COMMAND ${MAKEINDEX_COMPILER} refman.idx
		COMMAND ${PDFTEX_CMD} refman.tex
		COMMAND ${PDFTEX_CMD} refman.tex
		COMMAND ${CMAKE_COMMAND} -E copy_if_different refman.pdf ${ORBITER_BINARY_SDK_DIR}/doc/API_Reference.pdf
		WORKING_DIRECTORY ./latex
		JOB_POOL latex
	)
	list(APPEND api_ref_out ${ORBITER_BINARY_SDK_DIR}/doc/API_Reference.pdf)
endif()
add_custom_target(APIReference
	DEPENDS ${api_ref_out}
)
add_dependencies(${OrbiterTgt}
	APIReference
)

install(FILES ${out} ${ORBITER_BINARY_SDK_DIR}/doc/API_Reference.pdf
	DESTINATION ${ORBITER_INSTALL_SDK_DIR}/doc
)
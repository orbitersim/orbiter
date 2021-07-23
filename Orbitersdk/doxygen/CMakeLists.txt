configure_file(${CMAKE_CURRENT_SOURCE_DIR}/Doxyfile.in ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile
)

set(out ${ORBITER_BINARY_SDK_DIR}/doc/API_Reference.chm)

add_custom_command(
	OUTPUT ${out}
	COMMAND ${DOXYGEN_EXECUTABLE} Doxyfile && (echo .) || (echo .)
	COMMAND ${CMAKE_COMMAND} -E copy_if_different html/API_Reference.chm ${out}
	JOB_POOL latex
)
add_custom_command(
	OUTPUT ${ORBITER_BINARY_SDK_DIR}/doc/API_Reference.pdf
	DEPENDS ${out}
	COMMAND ${PDFLATEX_COMPILER} refman.tex && (echo .) || (echo .)
	COMMAND ${MAKEINDEX_COMPILER} refman.idx
	COMMAND ${PDFLATEX_COMPILER} refman.tex && (echo .) || (echo .)
	COMMAND ${PDFLATEX_COMPILER} refman.tex && (echo .) || (echo .)
	COMMAND ${CMAKE_COMMAND} -E copy_if_different refman.pdf ${ORBITER_BINARY_SDK_DIR}/doc/API_Reference.pdf
	WORKING_DIRECTORY ./latex
	JOB_POOL latex
)
add_custom_target(APIReference
	DEPENDS ${out} ${ORBITER_BINARY_SDK_DIR}/doc/API_Reference.pdf
)
add_dependencies(Orbiter
	APIReference
)

install(FILES ${out} ${ORBITER_BINARY_SDK_DIR}/doc/API_Reference.pdf
	DESTINATION ${ORBITER_INSTALL_SDK_DIR}/doc
)
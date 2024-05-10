set(name "Orbiter User Manual")

set(src_path ${CMAKE_CURRENT_SOURCE_DIR}/${name}.tex)
set(out_path ${CMAKE_CURRENT_BINARY_DIR}/${name}.pdf)
set(pdflatex_cmd ${PDFLATEX_COMPILER} --synctex=1 -output-directory=${CMAKE_CURRENT_BINARY_DIR} ${src_path})
set(bibtex_cmd ${BIBTEX_COMPILER} ${CMAKE_CURRENT_BINARY_DIR}/${name})

add_custom_command(
	OUTPUT ${out_path}
	COMMAND ${pdflatex_cmd}
	COMMAND ${bibtex_cmd}
	COMMAND ${pdflatex_cmd}
	DEPENDS ${src_path}
	WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
	JOB_POOL latex
)
add_custom_target(OrbiterUserManual
	DEPENDS ${out_path}
)
add_dependencies(${OrbiterTgt}
	OrbiterUserManual
)
set_target_properties(OrbiterUserManual
	PROPERTIES
	FOLDER Doc
)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/${name}.pdf
	DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/Doc
)

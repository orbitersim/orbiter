# not upstream: Linux edition of the Developer Manual, built instead of Doc/Orbiter Developer Manual on Linux
set(name "Orbiter Developer Manual")

set(src_path ${CMAKE_CURRENT_SOURCE_DIR}/${name}.tex)
set(out_path ${CMAKE_CURRENT_BINARY_DIR}/${name}.pdf)
set(pdflatex_cmd ${PDFLATEX_COMPILER} --synctex=1 -output-directory=${CMAKE_CURRENT_BINARY_DIR} ${src_path})
# TeX Live's bibtex won't write outside its working folder (openout_any = p)
set(bibtex_cmd ${CMAKE_COMMAND} -E env openout_any=a ${BIBTEX_COMPILER} ${CMAKE_CURRENT_BINARY_DIR}/${name})

file(GLOB tex_files ${CMAKE_CURRENT_SOURCE_DIR}/*.tex)

add_custom_command(
	OUTPUT ${out_path}
	COMMAND ${pdflatex_cmd}
	COMMAND ${bibtex_cmd}
	COMMAND ${pdflatex_cmd}
	DEPENDS ${tex_files}
	WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
	JOB_POOL latex
)
add_custom_target(OrbiterDeveloperManual
	DEPENDS ${out_path}
)
add_dependencies(${OrbiterTgt}
	OrbiterDeveloperManual
)
set_target_properties(OrbiterDeveloperManual
	PROPERTIES
	FOLDER Doc
)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/${name}.pdf
	DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/Doc
	RENAME "${name} (Linux).pdf"
)

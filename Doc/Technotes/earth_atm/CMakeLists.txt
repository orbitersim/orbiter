set(name earth_atm)

set(src_path ${CMAKE_CURRENT_SOURCE_DIR}/${name}.tex)
set(dvi_path ${CMAKE_CURRENT_BINARY_DIR}/${name}.dvi)
set(ps_path  ${CMAKE_CURRENT_BINARY_DIR}/${name}.ps)
set(out_path ${CMAKE_CURRENT_BINARY_DIR}/${name}.pdf)
set(latex_cmd ${LATEX_COMPILER} -output-directory=${CMAKE_CURRENT_BINARY_DIR} ${src_path})
set(bibtex_cmd ${BIBTEX_COMPILER} ${CMAKE_CURRENT_BINARY_DIR}/${name})
set(dvips_cmd ${DVIPS_CONVERTER} ${dvi_path} -o ${ps_path})
set(pspdf_cmd ${PS2PDF_CONVERTER} ${ps_path} ${out_path})

add_custom_command(
	OUTPUT ${out_path}
	COMMAND ${latex_cmd}
	COMMAND ${bibtex_cmd}
	COMMAND ${latex_cmd}
	COMMAND ${latex_cmd}
	COMMAND ${dvips_cmd}
	COMMAND ${pspdf_cmd}
	DEPENDS ${src_path}
	WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
	JOB_POOL latex
)
add_custom_target(EarthAtmTechnote
	DEPENDS ${out_path}
)
add_dependencies(Orbiter
	EarthAtmTechnote
)
set_target_properties(EarthAtmTechnote
	PROPERTIES
	FOLDER Doc/Technotes
)

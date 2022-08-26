# Atlantis.pdf --------------------------------------------------------

set(document "Atlantis")
set(outdir ${ORBITER_BINARY_DOC_DIR})
set(src ${CMAKE_CURRENT_SOURCE_DIR}/${document}.odt)
set(out ${outdir}/${document}.pdf)

add_custom_command(
	OUTPUT ${out}
	COMMAND ${ODT_TO_PDF_COMPILER} --headless --convert-to pdf --outdir ${outdir} ${src}
	DEPENDS ${src}
	JOB_POOL soffice
)
add_custom_target(AtlantisDoc
	DEPENDS ${out}
)
add_dependencies(Atlantis
	AtlantisDoc
)
set_target_properties(AtlantisDoc
	PROPERTIES
	FOLDER Doc
)
install(FILES ${out}
	DESTINATION ${ORBITER_INSTALL_DOC_DIR}
)

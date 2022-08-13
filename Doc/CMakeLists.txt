# Orbiter.pdf ------------------------------------------------------------

odt_to_pdf_arglist("Orbiter" arglist src out)
add_custom_command(
	OUTPUT ${out}
	COMMAND ${ODT_TO_PDF_COMPILER} ${arglist}
	DEPENDS ${src}
	JOB_POOL soffice
)
add_custom_target(OrbiterDoc
	DEPENDS ${out}
)
add_dependencies(${OrbiterTgt}
	OrbiterDoc
)
set_target_properties(OrbiterDoc
	PROPERTIES
	FOLDER Doc
)
install(FILES ${out}
	DESTINATION ${ORBITER_INSTALL_DOC_DIR}
)

# OrbiterConfig.pdf ------------------------------------------------------

odt_to_pdf_arglist("OrbiterConfig" arglist src out)
add_custom_command(
	OUTPUT ${out}
	COMMAND ${ODT_TO_PDF_COMPILER} ${arglist}
	DEPENDS ${src}
	JOB_POOL soffice
)
add_custom_target(OrbiterConfigDoc
	DEPENDS ${out}
)
add_dependencies(${OrbiterTgt}
	OrbiterConfigDoc
)
set_target_properties(OrbiterConfigDoc
	PROPERTIES
	FOLDER Doc
)
install(FILES ${out}
	DESTINATION ${ORBITER_INSTALL_DOC_DIR}
)

# PlanetTextures.pdf -----------------------------------------------------

odt_to_pdf_arglist("PlanetTextures" arglist src out)
add_custom_command(
	OUTPUT ${out}
	COMMAND ${ODT_TO_PDF_COMPILER} ${arglist}
	DEPENDS ${src}
	JOB_POOL soffice
)
add_custom_target(PlanetTexturesDoc
	DEPENDS ${out}
)
add_dependencies(${OrbiterTgt}
	PlanetTexturesDoc
)
set_target_properties(PlanetTexturesDoc
	PROPERTIES
	FOLDER Doc
)
install(FILES ${out}
	DESTINATION ${ORBITER_INSTALL_DOC_DIR}
)

# Credit.pdf -------------------------------------------------------------

doc_to_pdf_arglist("Credit" arglist src out)
add_custom_command(
	OUTPUT ${out}
	COMMAND ${DOC_TO_PDF_COMPILER} ${arglist}
	DEPENDS ${src}
	JOB_POOL soffice
)
add_custom_target(OrbiterCreditDoc
	DEPENDS ${out}
)
add_dependencies(OrbiterDoc
	OrbiterCreditDoc
)
install(FILES ${out}
	DESTINATION ${ORBITER_INSTALL_DOC_DIR}
)

# Technotes --------------------------------------------------------------

add_subdirectory(Technotes)

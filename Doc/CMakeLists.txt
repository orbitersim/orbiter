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

# ScenarioEditor.pdf -----------------------------------------------------

doc_to_pdf_arglist("ScenarioEditor" arglist src out)
add_custom_command(
	OUTPUT ${out}
	COMMAND ${DOC_TO_PDF_COMPILER} ${arglist}
	DEPENDS ${src}
	JOB_POOL soffice
)
add_custom_target(ScenarioEditorDoc
	DEPENDS ${out}
)
add_dependencies(OrbiterDoc
	ScenarioEditorDoc
)
install(FILES ${out}
	DESTINATION ${ORBITER_INSTALL_DOC_DIR}
)

# Change.pdf -------------------------------------------------------------

doc_to_pdf_arglist("Change" arglist src out)
add_custom_command(
	OUTPUT ${out}
	COMMAND ${DOC_TO_PDF_COMPILER} ${arglist}
	DEPENDS ${src}
	JOB_POOL soffice
)
add_custom_target(OrbiterChangeDoc
	DEPENDS ${out}
)
add_dependencies(OrbiterDoc
	OrbiterChangeDoc
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

# DeltaGlider.pdf --------------------------------------------------------

# Note: DeltaGlider doc should be moved to the DeltaGlider package build
odt_to_pdf_arglist("DeltaGlider" arglist src out)
add_custom_command(
	OUTPUT ${out}
	COMMAND ${ODT_TO_PDF_COMPILER} ${arglist}
	DEPENDS ${src}
	JOB_POOL soffice
)
add_custom_target(DeltaGliderDoc
	DEPENDS ${out}
)
add_dependencies(OrbiterDoc
	DeltaGliderDoc
)
set_target_properties(DeltaGliderDoc
	PROPERTIES
	FOLDER Doc
)
install(FILES ${out}
	DESTINATION ${ORBITER_INSTALL_DOC_DIR}
)

# ShuttleA.pdf -----------------------------------------------------------

doc_to_pdf_arglist("ShuttleA" arglist src out)
add_custom_command(
	OUTPUT ${out}
	COMMAND ${DOC_TO_PDF_COMPILER} ${arglist}
	DEPENDS ${src}
	JOB_POOL soffice
)
add_custom_target(ShuttleADoc
	DEPENDS ${out}
)
add_dependencies(OrbiterDoc
	ShuttleADoc
)
install(FILES ${out}
	DESTINATION ${ORBITER_INSTALL_DOC_DIR}
)

# Atlantis.pdf -----------------------------------------------------------

# Note: Atlantis doc should be moved to the Atlantis package build
odt_to_pdf_arglist("Atlantis" arglist src out)
add_custom_command(
	OUTPUT ${out}
	COMMAND ${ODT_TO_PDF_COMPILER} ${arglist}
	DEPENDS ${src}
	JOB_POOL soffice
)
add_custom_target(AtlantisDoc
	DEPENDS ${out}
)
add_dependencies(OrbiterDoc
	AtlantisDoc
)
set_target_properties(AtlantisDoc
	PROPERTIES
	FOLDER Doc
)
install(FILES ${out}
	DESTINATION ${ORBITER_INSTALL_DOC_DIR}
)

# Atlantis_MMU_Sat_30.pdf ------------------------------------------------

set(name Atlantis_MMU_Sat_30.pdf)
set(out ${ORBITER_BINARY_ROOT_DIR}/Doc/${name})
add_custom_command(
	OUTPUT ${out}
	COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/${name} ${out}
)
add_custom_target(DocMMU
	DEPENDS ${out}
)
add_dependencies(OrbiterDoc
	DocMMU
)
install(FILES ${out}
	DESTINATION ${ORBITER_INSTALL_DOC_DIR}
)

# Technotes --------------------------------------------------------------

add_subdirectory(Technotes)

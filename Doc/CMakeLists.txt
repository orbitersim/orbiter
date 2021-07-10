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
add_dependencies(Orbiter
	OrbiterDoc
)
set_target_properties(OrbiterDoc
	PROPERTIES
	FOLDER Doc
)

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
add_dependencies(Orbiter
	OrbiterConfigDoc
)
set_target_properties(OrbiterConfigDoc
	PROPERTIES
	FOLDER Doc
)

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
add_dependencies(Orbiter
	PlanetTexturesDoc
)
set_target_properties(PlanetTexturesDoc
	PROPERTIES
	FOLDER Doc
)

# TODO: Change not yet available as ODT
#odt_to_pdf_arglist("Change" arglist src out)
#add_custom_command(
#	OUTPUT ${out}
#	COMMAND ${ODT_TO_PDF_COMPILER} ${arglist}
#	DEPENDS ${src}
#)
#add_custom_target(OrbiterChangeDoc
#	DEPENDS ${out}
#)
#add_dependencies(Orbiter
#	OrbiterChangeDoc
#)

# TODO: Credit not yet available as ODT
#odt_to_pdf_arglist("Credit" arglist src out)
#add_custom_command(
#	OUTPUT ${out}
#	COMMAND ${ODT_TO_PDF_COMPILER} ${arglist}
#	DEPENDS ${src}
#)
#add_custom_target(OrbiterCreditDoc
#	DEPENDS ${out}
#)
#add_dependencies(Orbiter
#	OrbiterCreditDoc
#)

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
add_dependencies(Orbiter
	DeltaGliderDoc
)
set_target_properties(DeltaGliderDoc
	PROPERTIES
	FOLDER Doc
)

if(ORBITER_MAKE_TECHNOTES)
	add_subdirectory(Technotes)
endif()

# RecorderRef.pdf --------------------------------------------------------

doc_to_pdf_arglist("RecorderRef" arglist src out)
add_custom_command(
	OUTPUT ${out}
	COMMAND ${DOC_TO_PDF_COMPILER} ${arglist}
	DEPENDS ${src}
	JOB_POOL soffice
)
add_custom_target(RecorderRefDoc
	DEPENDS ${out}
)
add_dependencies(OrbiterDoc
	RecorderRefDoc
)
install(FILES ${out}
	DESTINATION ${ORBITER_INSTALL_DOC_DIR}/Technotes
)

if(LATEX_FOUND)
	add_subdirectory(composite)
	add_subdirectory(distmass)
	add_subdirectory(dynamics)
	add_subdirectory(earth_atm)
	add_subdirectory(gravity)
	add_subdirectory(lighting)
	add_subdirectory(precession)
endif()

# Atlantis_MMU_Sat_30.pdf ------------------------------------------------

set(document "Atlantis_MMU_Sat_30.pdf")
set(out ${ORBITER_BINARY_DOC_DIR}/${document})
add_custom_command(
	OUTPUT ${out}
	COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/${document} ${out}
)
add_custom_target(MMUDoc
	DEPENDS ${out}
)
add_dependencies(MMU
	MMUDoc
)
install(FILES ${out}
	DESTINATION ${ORBITER_INSTALL_DOC_DIR}
)

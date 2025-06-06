# Copyright (c) Martin Schweiger
# Licensed under the MIT License

add_subdirectory(Atlantis)
add_subdirectory(Atlantis_SRB)
add_subdirectory(Atlantis_Tank)
add_subdirectory(AtlantisConfig)
if(ORBITER_MAKE_DOC)
	add_subdirectory(Help)
endif()

set(ORBITER_SMAPLES ${ORBITER_INSTALL_ROOT_DIR}/Orbitersdk/samples)




# ==========================================================================
# SDK Sample Installation
# ==========================================================================

set(Exclude
	PATTERN CMakeLists.txt EXCLUDE
)

install(FILES Common.cpp DESTINATION ${ORBITER_SMAPLES}/Atlantis)
install(DIRECTORY Bitmaps DESTINATION ${ORBITER_SMAPLES}/Atlantis ${Exclude})
install(DIRECTORY Data DESTINATION ${ORBITER_SMAPLES}/Atlantis ${Exclude})
install(DIRECTORY Help DESTINATION ${ORBITER_SMAPLES}/Atlantis ${Exclude})

set(IgnoreFiles
	${Exclude}
	PATTERN Textures EXCLUDE
	PATTERN Textures2 EXCLUDE
	PATTERN Config EXCLUDE
	PATTERN Meshes EXCLUDE
	PATTERN Thumbnail EXCLUDE
)

install(DIRECTORY Atlantis/ DESTINATION ${ORBITER_SMAPLES}/Atlantis/Atlantis ${IgnoreFiles})
install(DIRECTORY Atlantis_SRB/ DESTINATION ${ORBITER_SMAPLES}/Atlantis/Atlantis_SRB ${IgnoreFiles})
install(DIRECTORY Atlantis_Tank/ DESTINATION ${ORBITER_SMAPLES}/Atlantis/Atlantis_Tank ${IgnoreFiles})
install(DIRECTORY AtlantisConfig/ DESTINATION ${ORBITER_SMAPLES}/Atlantis/AtlantisConfig ${IgnoreFiles})

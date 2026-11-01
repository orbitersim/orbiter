# Copyright (c) Martin Schweiger
# Licensed under the MIT License

set(CELBODY "Deimos")

# Deimos.dll is a prebuilt 32-bit module; its source is not in the repository. A 64-bit Orbiter cannot load
# it, so 64-bit builds neither copy nor install it and the body follows the Kepler elements in its config
# instead (CelestialBody::RegisterModule logs the fallback). TODO: build a 64-bit module once the source
# is available.
if(NOT BUILD64)
	add_custom_command(
		OUTPUT ${ORBITER_BINARY_CELBODY_DIR}/${CELBODY}.dll
		COMMAND ${CMAKE_COMMAND} -E make_directory ${ORBITER_BINARY_CELBODY_DIR}
		COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/${CELBODY}.dll ${ORBITER_BINARY_CELBODY_DIR}
		DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${CELBODY}.dll
	)
	set(CELBODY_DLL ${ORBITER_BINARY_CELBODY_DIR}/${CELBODY}.dll)
else()
	set(CELBODY_DLL "")
endif()

srcdir_to_tgtlist("${CMAKE_CURRENT_SOURCE_DIR}/Config" "${ORBITER_BINARY_CONFIG_DIR}" srclist tgtlist)
add_custom_command(
	OUTPUT ${tgtlist}
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_SOURCE_DIR}/Config/ ${ORBITER_BINARY_CONFIG_DIR}
	DEPENDS ${srclist}
)
add_custom_target(${CELBODY}
	DEPENDS ${tgtlist} ${CELBODY_DLL}
)
add_dependencies(Sol
	${CELBODY}
)

set_target_properties(${CELBODY}
	PROPERTIES
	FOLDER Celbody
)

# Installation
if(NOT BUILD64)
	install(PROGRAMS
		${CELBODY}.dll
		DESTINATION ${ORBITER_INSTALL_CELBODY_DIR}
	)
endif()
install(DIRECTORY Config
	DESTINATION ${ORBITER_INSTALL_ROOT_DIR}
)

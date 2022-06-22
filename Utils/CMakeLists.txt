# Copyright (c) Martin Schweiger
# Licensed under the MIT License

add_subdirectory(Date)
add_subdirectory(fchecksum)
add_subdirectory(meshc)
add_subdirectory(Pltex)
add_subdirectory(Shipedit)
add_subdirectory(scramble)
add_subdirectory(texpack)

# We do this as an external project to invoke x64 toolchain
ExternalProject_Add(plsplit
	PREFIX ${CMAKE_CURRENT_BINARY_DIR}/plsplit
	BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR}/plsplit
	SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/plsplit
	INSTALL_DIR ${ORBITER_INSTALL_UTILS_DIR}
	CMAKE_GENERATOR "Visual Studio 16 2019" 
	CMAKE_GENERATOR_PLATFORM "x64"
	CMAKE_ARGS -DCMAKE_INSTALL_PREFIX:PATH=${CMAKE_INSTALL_PREFIX} -DINSTALLDIR=${ORBITER_INSTALL_UTILS_DIR}
	BUILD_COMMAND ${CMAKE_COMMAND} --build ${CMAKE_CURRENT_BINARY_DIR}/plsplit --config Release
	INSTALL_COMMAND ""
)
# Install project output from here instead from inside the external project
# to make cpack aware of it
install(PROGRAMS
	${CMAKE_CURRENT_BINARY_DIR}/plsplit/plsplit.exe
	plsplit/DxTex.exe
	DESTINATION ${ORBITER_INSTALL_UTILS_DIR}
)

# tileedit is another 64-bit project. This also requires
# Qt5 so will only be built if that is available
if (NOT ${Qt5_x64_DIR} STREQUAL Qt5_x64_DIR_NOTFOUND)
	ExternalProject_Add(tileedit_proj
		PREFIX ${CMAKE_CURRENT_BINARY_DIR}/tileedit
		BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR}/tileedit
		SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/tileedit
		INSTALL_DIR ${ORBITER_INSTALL_UTILS_DIR}
		CMAKE_GENERATOR "Visual Studio 16 2019"
		CMAKE_GENERATOR_PLATFORM "x64"
		CMAKE_ARGS -DCMAKE_CFG_INTDIR=Release -DQTDIR:PATH=${Qt5_x64_DIR} -DCMAKE_INSTALL_PREFIX:PATH=${CMAKE_INSTALL_PREFIX} -DINSTALLDIR=${ORBITER_INSTALL_UTILS_DIR}
		BUILD_COMMAND ${CMAKE_COMMAND} --build ${CMAKE_CURRENT_BINARY_DIR}/tileedit --config Release
		INSTALL_COMMAND ""
	)
	# Install project output from here instead from inside the external project
	# to make cpack aware of it
	install(DIRECTORY
		${CMAKE_CURRENT_BINARY_DIR}/tileedit/src/tmp/
		DESTINATION ${ORBITER_INSTALL_UTILS_DIR}
	)
endif()

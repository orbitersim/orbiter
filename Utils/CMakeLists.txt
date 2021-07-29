# Copyright (c) Martin Schweiger
# Licensed under the MIT License

add_subdirectory(Date)
add_subdirectory(fchecksum)
add_subdirectory(meshc)
add_subdirectory(Pltex)
add_subdirectory(Shipedit)
add_subdirectory(scramble)
add_subdirectory(texpack)
add_subdirectory(tileedit/qt)

# We do this as an external project to invoke x64 toolchain
ExternalProject_Add(plsplit
	PREFIX ${CMAKE_CURRENT_BINARY_DIR}/plsplit
	BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR}/plsplit
	SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/plsplit
	INSTALL_DIR ${ORBITER_INSTALL_UTILS_DIR}
	CMAKE_GENERATOR "Visual Studio 16 2019" 
	CMAKE_GENERATOR_PLATFORM "x64"
	CMAKE_ARGS -DCMAKE_INSTALL_PREFIX:PATH=${ORBITER_INSTALL_UTILS_DIR}
)

# tileedit is another 64-bit project. This also requires
# Qt5 so will only be built if that is available
if (Qt5_FOUND)
ExternalProject_Add(tileedit_proj
	PREFIX ${CMAKE_CURRENT_BINARY_DIR}/tileedit/qt
	BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR}/tileedit/qt
	SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/tileedit/qt
	INSTALL_DIR ${ORBITER_INSTALL_UTILS_DIR}
	CMAKE_GENERATOR "Visual Studio 16 2019"
	CMAKE_GENERATOR_PLATFORM "x64"
	CMAKE_ARGS -DCMAKE_INSTALL_PREFIX:PATH=${ORBITER_INSTALL_UTILS_DIR} -DQTDIR:PATH=${Qt5_DIR}
#	BUILD_COMMAND msbuild ${CMAKE_CURRENT_BINARY_DIR}/tileedit/qt/tileedit.sln
)
endif()

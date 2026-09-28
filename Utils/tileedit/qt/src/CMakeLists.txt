# Copyright (c) Martin Schweiger
# Licensed under the MIT License

if(WIN32) # Windows: upstream
set(CMAKE_CXX_STANDARD 11)
else() # Linux port
# not upstream: C++20 like the rest of the port (Qt 6 needs C++17 at least)
set(CMAKE_CXX_STANDARD 20)
endif() # WIN32
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTORCC ON)
set(CMAKE_AUTOUIC ON)

if(CMAKE_VERSION VERSION_LESS "3.7.0")
    set(CMAKE_INCLUDE_CURRENT_DIR ON)
endif()

if(NOT WIN32) # Linux port
# system libpng replaces the prebuilt extern/libpng
find_package(PNG REQUIRED)

endif() # NOT WIN32
add_executable(tileedit
	tileedit.cpp
	tileedit.qrc
	cmap.cpp
	colorbar.cpp
	ddsread.cpp
	dlgconfig.cpp
	dlgelevconfig.cpp
	dlgelevexport.cpp
	dlgelevimport.cpp
	dlgsurfimport.cpp
	dxt_io.cpp
	elevtile.cpp
	elv_io.cpp
	imagetools.cpp
	main.cpp
	tile.cpp
	tileblock.cpp
	tilecanvas.cpp
	ZTreeMgr.cpp
	tileedit.ui
	dlgConfig.ui
	dlgElevConfig.ui
	dlgElevExport.ui
	dlgElevImport.ui
	dlgSurfImport.ui
)

add_dependencies(tileedit
	fastdxt
)

if(NOT WIN32) # Linux port
# extern/libpng and extern/zlib headers left out: PNG::PNG and the zlib target carry their own
endif() # NOT WIN32
if(WIN32) # Windows: upstream
target_include_directories(tileedit PUBLIC
	${CMAKE_CURRENT_SOURCE_DIR}
	${CMAKE_CURRENT_SOURCE_DIR}/../extern/libpng/include
	${CMAKE_CURRENT_SOURCE_DIR}/../extern/zlib/include
	${CMAKE_CURRENT_SOURCE_DIR}/../extern/fastdxt
)
else() # Linux port
target_include_directories(tileedit PUBLIC
	${CMAKE_CURRENT_SOURCE_DIR}
	${CMAKE_CURRENT_SOURCE_DIR}/../extern/fastdxt
	${ORBITER_SOURCE_SDK_INCLUDE_DIR} # not upstream: OrbiterPlatform.h stands in for windows.h (DWORD, BYTE, ...)
)
endif() # WIN32

if(NOT WIN32) # Linux port
# prebuilt Windows zlib.lib/libpng16_static.lib left out: the repo's zlib target and system libpng
endif() # NOT WIN32
if(WIN32) # Windows: upstream
target_link_libraries(tileedit
	Qt5::Widgets
	Qt5::Core
	Qt5::Gui
	${CMAKE_CURRENT_SOURCE_DIR}/../extern/zlib/lib/zlib.lib
	${CMAKE_CURRENT_SOURCE_DIR}/../extern/libpng/lib/libpng16_static.lib
	${FASTDXT_LIB}
)
else() # Linux port
target_link_libraries(tileedit
	Qt6::Widgets
	Qt6::Core
	Qt6::Gui
	zlib
	PNG::PNG
	${FASTDXT_LIB}
)
endif() # WIN32

if(NOT WIN32) # Linux port
# WIN32_EXECUTABLE left out: no GUI/console subsystem split on Linux
# not upstream: build-tree RPATH reaches z.so in the build root (upstream linked zlib statically); install keeps $ORIGIN/..
endif() # NOT WIN32
if(WIN32) # Windows: upstream
set_target_properties(tileedit
	PROPERTIES
	WIN32_EXECUTABLE true
)
else() # Linux port
set_target_properties(tileedit
	PROPERTIES
	BUILD_WITH_INSTALL_RPATH OFF
)
endif() # WIN32

if(WIN32) # Windows: upstream
# deploy the Qt libraries
add_custom_command(TARGET tileedit
	POST_BUILD
	COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_CURRENT_BINARY_DIR}/tmp
	COMMAND ${CMAKE_COMMAND} -E copy $<TARGET_FILE:tileedit> ${CMAKE_CURRENT_BINARY_DIR}/tmp
	COMMAND ${Qt5_DIR}/../../../bin/windeployqt ${CMAKE_CURRENT_BINARY_DIR}/tmp/$<TARGET_FILE_NAME:tileedit>
)
else() # Linux port
# windeployqt left out: Qt 6 comes from the system, so no tmp/ deploy copy is made
endif() # WIN32

# Installation - this is currently skipped

if(NOT WIN32) # Linux port
# INSTALLDIR came from the ExternalProject, which is gone
endif() # NOT WIN32
if(WIN32) # Windows: upstream
install(TARGETS
	tileedit
	RUNTIME
	DESTINATION ${INSTALLDIR}
)
# install all dlls that windeployqt has dropped in the build dir
file(GLOB QT_DEPLOY_DLL
	${CMAKE_CURRENT_BINARY_DIR}/tmp/*.dll
)
install(FILES ${QT_DEPLOY_DLL}
	DESTINATION ${INSTALLDIR}
)
else() # Linux port
install(TARGETS
	tileedit
	RUNTIME
	DESTINATION ${ORBITER_INSTALL_UTILS_DIR}
)
endif() # WIN32
if(NOT WIN32) # Linux port
# windeployqt DLL install left out: no deployed Qt libraries
endif() # NOT WIN32

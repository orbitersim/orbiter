# Copyright (c) Martin Schweiger
# Licensed under the MIT License

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTORCC ON)
set(CMAKE_AUTOUIC ON)

if(CMAKE_VERSION VERSION_LESS "3.7.0")
    set(CMAKE_INCLUDE_CURRENT_DIR ON)
endif()

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

target_include_directories(tileedit PUBLIC
	${CMAKE_CURRENT_SOURCE_DIR}
	${CMAKE_CURRENT_SOURCE_DIR}/../extern/libpng/include
	${CMAKE_CURRENT_SOURCE_DIR}/../extern/zlib/include
	${CMAKE_CURRENT_SOURCE_DIR}/../extern/fastdxt
)

target_link_libraries(tileedit
	Qt5::Widgets
	Qt5::Core
	Qt5::Gui
	${CMAKE_CURRENT_SOURCE_DIR}/../extern/zlib/lib/zlib.lib
	${CMAKE_CURRENT_SOURCE_DIR}/../extern/libpng/lib/libpng16_static.lib
	${FASTDXT_LIB}
)

set_target_properties(tileedit
	PROPERTIES
	RUNTIME_OUTPUT_DIRECTORY_DEBUG ${CMAKE_CURRENT_BINARY_DIR}
	RUNTIME_OUTPUT_DIRECTORY_RELEASE ${CMAKE_CURRENT_BINARY_DIR}
	WIN32_EXECUTABLE true
)

add_custom_command(TARGET tileedit
	POST_BUILD
	COMMAND ${Qt5_DIR}/../../../bin/windeployqt $<TARGET_FILE:tileedit>
)

# Installation
install(TARGETS
	tileedit
	RUNTIME
	DESTINATION ${CMAKE_INSTALL_PREFIX}
)

# install all dlls that windeployqt has dropped in the build dir
file(GLOB QT_DEPLOY_DLL
	${CMAKE_CURRENT_BINARY_DIR}/*.dll
)
install(FILES ${QT_DEPLOY_DLL}
	DESTINATION ${CMAKE_INSTALL_PREFIX}
)

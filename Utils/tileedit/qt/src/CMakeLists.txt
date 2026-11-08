# Copyright (c) Martin Schweiger
# Licensed under the MIT License

set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTORCC ON)
set(CMAKE_AUTOUIC ON)

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

target_compile_features(tileedit PRIVATE cxx_std_17)

target_include_directories(tileedit PRIVATE
	${CMAKE_CURRENT_SOURCE_DIR}
)

target_link_libraries(tileedit PRIVATE
	Qt${QT_VERSION_MAJOR}::Widgets
	Qt${QT_VERSION_MAJOR}::Gui
	Qt${QT_VERSION_MAJOR}::Core
	PNG::PNG
	ZLIB::ZLIB
	fastdxt
)

set_target_properties(tileedit
	PROPERTIES
	FOLDER Tools
	INSTALL_RPATH "@executable_path"
	INSTALL_RPATH_USE_LINK_PATH TRUE
)

if(DEFINED ORBITER_INSTALL_UTILS_DIR)
	install(TARGETS tileedit
		RUNTIME DESTINATION ${ORBITER_INSTALL_UTILS_DIR}
	)
endif()

# Copyright (c) Martin Schweiger
# Licensed under the MIT License

cmake_minimum_required(VERSION 3.15)

project (tileedit VERSION 2021.1)

set(FASTDXT_LIB $<TARGET_FILE:fastdxt>)

if(WIN32) # Windows: upstream
find_package(Qt5 QUIET COMPONENTS Widgets Core Gui
	HINTS ${QTDIR}
)
else() # Linux port
# Qt6 replaces Qt5; QTDIR hint left out: it came from the ExternalProject, Qt 6 is a system package
find_package(Qt6 QUIET COMPONENTS Widgets Core Gui)
endif() # WIN32

if(WIN32) # Windows: upstream
if(Qt5_FOUND)
	add_subdirectory(extern/fastdxt)
	add_subdirectory(src)
endif()
else() # Linux port
if(Qt6_FOUND)
	add_subdirectory(extern/fastdxt)
	add_subdirectory(src)
endif()
endif() # WIN32

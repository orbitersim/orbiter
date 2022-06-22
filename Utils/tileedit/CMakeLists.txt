# Copyright (c) Martin Schweiger
# Licensed under the MIT License

cmake_minimum_required(VERSION 3.15)

project (tileedit VERSION 2021.1)

set(FASTDXT_LIB $<TARGET_FILE:fastdxt>)

find_package(Qt5 QUIET COMPONENTS Widgets Core Gui
	HINTS ${QTDIR}
)

if(Qt5_FOUND)
	add_subdirectory(extern/fastdxt)
	add_subdirectory(src)
endif()

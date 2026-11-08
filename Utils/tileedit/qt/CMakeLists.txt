# Copyright (c) Martin Schweiger
# Licensed under the MIT License

cmake_minimum_required(VERSION 3.16)

project(tileedit VERSION 2021.1 LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

find_package(QT NAMES Qt6 Qt5 QUIET COMPONENTS Widgets)
if(QT_FOUND)
	if(QT_VERSION_MAJOR EQUAL 5)
		find_package(Qt5 5.15 QUIET COMPONENTS Widgets Gui Core)
	else()
		find_package(Qt${QT_VERSION_MAJOR} QUIET COMPONENTS Widgets Gui Core)
	endif()
endif()
find_package(PNG QUIET)
find_package(ZLIB QUIET)

if(QT_FOUND AND Qt${QT_VERSION_MAJOR}Widgets_FOUND AND PNG_FOUND AND ZLIB_FOUND)
	add_subdirectory(extern/fastdxt)
	add_subdirectory(src)
else()
	message(STATUS "tileedit: Qt 6 (or Qt 5.15), libpng or zlib not found, skipping")
endif()

# Copyright (c) Martin Schweiger
# Licensed under the MIT License

add_library(fastdxt STATIC
	dxt.cpp
	libdxt.cpp
	util.cpp
)

target_include_directories(fastdxt PUBLIC
	${CMAKE_CURRENT_SOURCE_DIR}
)

set_target_properties(fastdxt
	PROPERTIES
	FOLDER Tools
)

# Copyright (c) Martin Schweiger
# Licensed under the MIT License

add_subdirectory(GDIClient)

if(NOT BUILD64)
	add_subdirectory(D3D7Client)
endif()
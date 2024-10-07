# Copyright (c) Martin Schweiger
# Licensed under the MIT License

add_subdirectory(GDIClient)

if(ORBITER_BUILD_D3D9CLIENT)
	add_subdirectory(D3D9Client)
endif()

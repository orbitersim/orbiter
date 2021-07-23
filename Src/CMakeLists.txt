# Copyright (c) Martin Schweiger
# Licensed under the MIT License

add_subdirectory(Orbiter)
add_subdirectory(Orbitersdk)
add_subdirectory(DlgCtrl)
add_subdirectory(Celbody)
add_subdirectory(Vessel)
add_subdirectory(Module)
add_subdirectory(Plugin)

if(NOT ORBITER_BUILD_WITH_DX7)
	add_subdirectory(Orbiter_ng)
endif()
# Copyright (c) Martin Schweiger
# Licensed under the MIT License

# Static library containing the DLL entry point for Orbiter plugins.
# To be included by all Orbiter plugin DLLs.

add_library(Orbitersdk STATIC
	Orbitersdk.cpp
)

set_target_properties(Orbitersdk
	PROPERTIES
	FOLDER Core
)

# Installation: Orbiter addon entry interface library to SDK folder to be included by addons
install(TARGETS Orbitersdk
	ARCHIVE
	DESTINATION ${ORBITER_INSTALL_SDK_DIR}/lib
)

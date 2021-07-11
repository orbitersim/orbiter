# Static library containing the DLL entry point for Orbiter plugins.
# To be included by all Orbiter plugin DLLs.

add_library(Orbitersdk STATIC
	Orbitersdk.cpp
)

set_target_properties(Orbitersdk
	PROPERTIES
	FOLDER Core
)

# Copyright (c) Martin Schweiger
# Licensed under the MIT License

add_executable(texpack
	texpack.cpp
)

if(WIN32) # Windows: upstream
target_link_libraries(texpack
	Shlwapi.lib
	zlib
)
else() # Linux port
target_link_libraries(texpack
	# Shlwapi.lib left out: PathFileExists/FindFirstFile -> access/readdir
	zlib
)

target_include_directories(texpack
	PRIVATE ${ORBITER_SOURCE_SDK_INCLUDE_DIR} # not upstream: OrbiterPlatform.h for the windows.h integer types
)
endif() # WIN32

set_target_properties(texpack
	PROPERTIES
	FOLDER Tools
)

# Installation
install(TARGETS
	texpack
	RUNTIME
	DESTINATION ${ORBITER_INSTALL_UTILS_DIR}
)

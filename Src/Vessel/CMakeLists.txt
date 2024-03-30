# Copyright (c) Martin Schweiger
# Licensed under the MIT License

# The target location for all vessel modules
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${ORBITER_BINARY_VESSEL_DIR})

# Common tools for vessels
set(VESSEL_COMMON_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Common)

# The individual vessel projects
add_subdirectory(Atlantis)
add_subdirectory(Carina)
add_subdirectory(DeltaGlider)
add_subdirectory(Dragonfly)
add_subdirectory(HST)
add_subdirectory(ISS)
add_subdirectory(LDEF)
add_subdirectory(Leonardo)
add_subdirectory(MMU)
add_subdirectory(Quadcopter)
add_subdirectory(ScriptVessel)
add_subdirectory(ShuttleA)
add_subdirectory(ShuttlePB)
add_subdirectory(Solarsail)



# ==========================================================================
# SDK Sample Installation
# ==========================================================================

set(Exclude
	PATTERN CMakeLists.txt EXCLUDE
)

set(IgnoreFiles
	${Exclude}
	PATTERN Textures EXCLUDE
	PATTERN Textures2 EXCLUDE
	PATTERN Config EXCLUDE
	PATTERN Meshes EXCLUDE
	PATTERN Thumbnail EXCLUDE
)

install(DIRECTORY DeltaGlider HST Quadcopter DESTINATION ${ORBITER_INSTALL_SDK_DIR}/samples ${IgnoreFiles})


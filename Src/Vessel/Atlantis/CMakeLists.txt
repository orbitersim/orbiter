# Copyright (c) Martin Schweiger
# Licensed under the MIT License

add_subdirectory(Atlantis)
add_subdirectory(Atlantis_SRB)
add_subdirectory(Atlantis_Tank)
add_subdirectory(AtlantisConfig)
if(ORBITER_MAKE_DOC)
	add_subdirectory(Doc)
	add_subdirectory(Help)
endif()

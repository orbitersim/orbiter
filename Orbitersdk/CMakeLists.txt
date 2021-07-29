# Copyright (c) Martin Schweiger
# Licensed under the MIT License

add_subdirectory(samples)
if(ORBITER_MAKE_DOC)
	add_subdirectory(doc)
endif()
if(Doxygen_FOUND)
	add_subdirectory(doxygen)
endif()

install(DIRECTORY html include lib
	DESTINATION ${ORBITER_INSTALL_SDK_DIR}
)

add_subdirectory(samples)
add_subdirectory(doc)
if(Doxygen_FOUND)
	add_subdirectory(doxygen)
endif()

install(DIRECTORY html include lib
	DESTINATION ${ORBITER_INSTALL_SDK_DIR}
)
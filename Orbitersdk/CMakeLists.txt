# Copyright (c) Martin Schweiger
# Licensed under the MIT License

add_subdirectory(samples)
add_subdirectory(sample_build)

file(GLOB_RECURSE includes
	RELATIVE ${CMAKE_CURRENT_SOURCE_DIR}
	LIST_DIRECTORIES false
	include/*
)
foreach(fname ${includes})
	list(APPEND input
		${CMAKE_CURRENT_SOURCE_DIR}/${fname}
	)
	list(APPEND output
		${ORBITER_BINARY_SDK_DIR}/${fname}
	)
	list(APPEND commands
		COMMAND ${CMAKE_COMMAND} -E copy_if_different ${CMAKE_CURRENT_SOURCE_DIR}/${fname} ${ORBITER_BINARY_SDK_DIR}/${fname}
	)
endforeach()

add_custom_command(
	OUTPUT ${output}
	DEPENDS ${input}
	${commands}
)
add_custom_target(copy_sdk_include
	DEPENDS ${output}
)
add_dependencies(Orbitersdk
	copy_sdk_include
)

if(ORBITER_MAKE_DOC)
	if(Doxygen_FOUND)
		add_subdirectory(doxygen)
	endif()
endif()

install(DIRECTORY html include
	DESTINATION ${ORBITER_INSTALL_SDK_DIR}
)

# Copy Prebuild PDFs ------------------------------------------------------

install(FILES doc/BuildingSamples.pdf DESTINATION ${ORBITER_INSTALL_SDK_DIR}/doc)


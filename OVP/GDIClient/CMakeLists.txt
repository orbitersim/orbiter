set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${ORBITER_BINARY_MODULE_DIR}/Server)

add_definitions(-DGDICLIENT_EXPORTS)

add_library(GDIClient SHARED
	GDIClient.cpp
)

target_include_directories(GDIClient
	PUBLIC ${CMAKE_SOURCE_DIR}/Orbitersdk/include
	PUBLIC ${DX7SDK_INCLUDE_DIR}
)

add_dependencies(GDIClient
	Orbiter
	Orbitersdk
)

target_link_libraries(GDIClient
	${ORBITER_LIB}
	${ORBITER_SDK_LIB}
	${DX7SDK_LIB_DIR}/dxguid.lib
	${DX7SDK_LIB_DIR}/d3dim.lib
	${DX7SDK_LIB_DIR}/ddraw.lib
	${DX7SDK_LIB_DIR}/dinput.lib
)

# Installation
install(TARGETS
	GDIClient
	RUNTIME
	DESTINATION ${ORBITER_INSTALL_MODULE_DIR}/Server
)

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${ORBITER_BINARY_MODULE_DIR})

add_library(Rcontrol SHARED
	Rcontrol.cpp
	Rcontrol.rc
)

target_include_directories(Rcontrol
	PUBLIC ${ORBITER_SOURCE_SDK_INCLUDE_DIR}
)

target_link_libraries(Rcontrol
	${ORBITER_LIB}
	${ORBITER_SDK_LIB}
	${ORBITER_DLGCTRL_LIB}
)

add_dependencies(Rcontrol
	Orbiter
	Orbitersdk
)

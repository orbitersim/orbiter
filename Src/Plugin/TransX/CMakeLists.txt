add_library(TransX SHARED
	BodyCache.cpp
	BodyProvider.cpp
    basefunction.cpp
    doublelink.cpp
    globals.cpp
    graph.cpp
    intercept.cpp
    mapfunction.cpp
    mfdfunction.cpp
    mfdvarhandler.cpp
    mfdvariable.cpp
    mfdvartypes.cpp
    orbitelements.cpp
    parser.cpp
    planfunction.cpp
    shiplist.cpp
    transx.cpp
    TransXFunction.cpp
    transxstate.cpp
    viewstate.cpp
    TransX.rc
)

target_include_directories(TransX
	PUBLIC ${ORBITER_SOURCE_SDK_INCLUDE_DIR}
	PUBLIC ${MODULE_COMMON_DIR}
)

target_link_libraries(TransX
	${ORBITER_LIB}
	${ORBITER_SDK_LIB}
)

add_dependencies(TransX
	${OrbiterTgt}
	Orbitersdk
)

set_target_properties(TransX
	PROPERTIES
	FOLDER Modules
)

add_custom_command(
    TARGET TransX PRE_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_SOURCE_DIR}/Doc ${ORBITER_BINARY_DOC_DIR}/TransX
)

# Installation
install(TARGETS TransX
	RUNTIME
	DESTINATION ${ORBITER_INSTALL_PLUGIN_DIR}
)
install(DIRECTORY ${ORBITER_BINARY_DOC_DIR}/TransX
    DESTINATION ${ORBITER_INSTALL_DOC_DIR}
)

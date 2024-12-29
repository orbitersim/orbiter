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

# copy only the needed txt files and leave the old pdf manuals and source doc
add_custom_command(
    TARGET TransX PRE_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/Doc/TransX\ Licence.txt ${ORBITER_BINARY_DOC_DIR}/TransX/TransX\ Licence.txt
    COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/Doc/TransX\ readme.txt ${ORBITER_BINARY_DOC_DIR}/TransX/TransX\ readme.txt
)

# Installation
install(TARGETS TransX
	RUNTIME
	DESTINATION ${ORBITER_INSTALL_PLUGIN_DIR}
)
install(DIRECTORY ${ORBITER_BINARY_DOC_DIR}/TransX
    DESTINATION ${ORBITER_INSTALL_DOC_DIR}
)

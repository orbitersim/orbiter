# Licensed under the MIT License


# =============================================================================
# TransX
# =============================================================================

set(PRJ TransX)

set(Sources
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
)

set(Headers
	BodyCache.h
	BodyProvider.h
    basefunction.h
    doublelink.h
    graph.h
    mfd.h
    cmdnugget.h
    intercept.h
    mapfunction.h
    mfdfunction.h
    mfdvarhandler.h
    mfdvariable.h
    mfdvartypes.h
    orbitelements.h
    parser.h
    planfunction.h
    shiplist.h
    transx.h
    TransXFunction.h
    transxstate.h
    viewstate.h
    resource.h
)
set(Other
	 TransX.rc
)
	
source_group(APIHeaders FILES ${APIHeaders})
source_group(Headers FILES ${Headers})
source_group(Sources FILES ${Sources})
source_group(Other FILES ${Other})

add_library(${PRJ} SHARED
	${Sources}
	${Headers}
    ${APIHeaders}
	${Other}
)


target_link_libraries(${PRJ} debug ${DebugLib})
target_link_libraries(${PRJ} optimized ${ReleaseLib})

set_target_properties(${PRJ}
	PROPERTIES
	RUNTIME_OUTPUT_DIRECTORY_DEBUG ${PLUGIN_DIR}
	RUNTIME_OUTPUT_DIRECTORY_RELEASE ${PLUGIN_DIR}
	FOLDER MFD
)

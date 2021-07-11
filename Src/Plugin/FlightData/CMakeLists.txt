add_library(FlightData SHARED
	FlightData.cpp
	FlightData.rc
	FDGraph.cpp
	../Common/Dialog/Graph.cpp
)

target_include_directories(FlightData
	PUBLIC ${ORBITER_SOURCE_SDK_INCLUDE_DIR}
	PUBLIC ${MODULE_COMMON_DIR}
)

target_link_libraries(FlightData
	${ORBITER_LIB}
	${ORBITER_SDK_LIB}
)

add_dependencies(FlightData
	Orbiter
	Orbitersdk
)

set_target_properties(FlightData
	PROPERTIES
	FOLDER Modules
)

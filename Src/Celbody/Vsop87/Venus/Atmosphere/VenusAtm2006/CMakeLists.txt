set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/Modules/Celbody/Venus/Atmosphere)

add_library(VenusAtm2006 SHARED
	VenusAtm2006.cpp
)

add_dependencies(VenusAtm2006
	Orbiter
	Orbitersdk
)

target_include_directories(VenusAtm2006
	PUBLIC ${CMAKE_SOURCE_DIR}/Orbitersdk/include
)

target_link_libraries(VenusAtm2006
	${ORBITER_LIB}
	${ORBITER_SDK_LIB}
)

set_target_properties(VenusAtm2006
	PROPERTIES
	FOLDER Celbody/Atmospheres
)

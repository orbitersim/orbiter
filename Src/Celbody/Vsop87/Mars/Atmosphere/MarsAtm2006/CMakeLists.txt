set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/Modules/Celbody/Mars/Atmosphere)

add_library(MarsAtm2006 SHARED
	MarsAtm2006.cpp
)

add_dependencies(MarsAtm2006
	Orbiter
	Orbitersdk
)

target_include_directories(MarsAtm2006
	PUBLIC ${CMAKE_SOURCE_DIR}/Orbitersdk/include
)

target_link_libraries(MarsAtm2006
	${ORBITER_LIB}
	${ORBITER_SDK_LIB}
)

set_target_properties(MarsAtm2006
	PROPERTIES
	FOLDER Celbody/Atmospheres
)

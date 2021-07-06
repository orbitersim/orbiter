set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/Modules)

add_library(Satsat SHARED
	Satsat.cpp
	Tass17.cpp
)

add_dependencies(Satsat
	Orbiter
	Orbitersdk
)

target_include_directories(Satsat
	PUBLIC ${CMAKE_SOURCE_DIR}/Orbitersdk/include
)

target_link_libraries(Satsat
	${ORBITER_LIB}
	${ORBITER_SDK_LIB}
)

add_custom_command(
	TARGET Satsat PRE_BUILD
	COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_BINARY_DIR}/Config/Saturn/Data
	COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/tass17.dat ${CMAKE_BINARY_DIR}/Config/Saturn/Data/
)

set(SATSAT_LIB $<TARGET_LINKER_FILE:Satsat>)

add_subdirectory(Mimas)
add_subdirectory(Enceladus)
add_subdirectory(Tethys)
add_subdirectory(Dione)
add_subdirectory(Rhea)
add_subdirectory(Titan)
add_subdirectory(Hyperion)
add_subdirectory(Iapetus)

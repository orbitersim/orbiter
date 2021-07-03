cmake_minimum_required(VERSION 3.10)

# Set the project name
project (Orbiter VERSION 2021.1)

set(OVP_DIR "${CMAKE_SOURCE_DIR}/OVP")
set(GDICLIENT_DIR "${OVP_DIR}/GDIClient")

set(EXTERN_PATH "${CMAKE_SOURCE_DIR}/Extern")

set(DX7SDK_PATH "${EXTERN_PATH}/mssdk_dx7")
set(DX7SDK_INCLUDE_DIR "${DX7SDK_PATH}/include")
set(DX7SDK_LIB_DIR "${DX7SDK_PATH}/lib")

set(ZLIB_PATH "${EXTERN_PATH}/Zlib")
set(ZLIB_INCLUDE_DIR "${ZLIB_PATH}/include")
set(ZLIB_LIB_DIR "${ZLIB_PATH}/lib")
set(ZLIB_BIN_DIR "${ZLIB_PATH}/bin")
set(ZLIB_LIBRARIES "${ZLIB_LIB_DIR}/zlibstatic.lib")

#set(CMAKE_PREFIX_PATH "${CMAKE_SOURCE_DIR}/../extern")

find_package(HTMLHelp)

if(MSVC)
  set(CMAKE_EXE_LINKER_FLAGS
      "${CMAKE_EXE_LINKER_FLAGS} /NODEFAULTLIB:\"LIBCMTD\"")
endif()

# Sub-projects
add_subdirectory(Src)
add_subdirectory(Core)
add_subdirectory(Utils)

add_custom_target(
	OrbiterProject
)

add_custom_command(
	TARGET OrbiterProject PRE_BUILD
	COMMAND ${CMAKE_COMMAND} -E copy_directory
	${CMAKE_SOURCE_DIR}/Scenarios/ $<TARGET_FILE_DIR:Orbiter>/Scenarios
)

add_custom_command(
	TARGET OrbiterProject PRE_BUILD
	COMMAND ${CMAKE_COMMAND} -E copy_directory
	${CMAKE_SOURCE_DIR}/Textures/ $<TARGET_FILE_DIR:Orbiter>/Textures
)
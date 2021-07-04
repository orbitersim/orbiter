cmake_minimum_required(VERSION 3.10)

# Set the project name
project (Orbiter VERSION 2021.1)

# Define directories and file paths for some common binaries
set(OVP_DIR "${CMAKE_SOURCE_DIR}/OVP")
set(GDICLIENT_DIR "${OVP_DIR}/GDIClient")

set(ORBITER_SOURCE_DIR ${CMAKE_SOURCE_DIR}/Src/Orbiter)
set(ORBITER_SOURCE_SDK_DIR ${CMAKE_SOURCE_DIR}/Orbitersdk)
set(ORBITER_SOURCE_SDK_INCLUDE_DIR ${ORBITER_SOURCE_SDK_DIR}/include)
set(ORBITER_SOURCE_MODULE_DIR ${ORBITER_SOURCE_SDK_DIR}/Module) # this will probably be rebased to Src
set(ORBITER_BINARY_DIR ${CMAKE_BINARY_DIR})
set(ORBITER_BINARY_MODULE_DIR ${ORBITER_BINARY_DIR}/Modules)
set(ORBITER_BINARY_CELBODY_DIR ${ORBITER_BINARY_MODULE_DIR}/Celbody)
set(ORBITER_BINARY_VESSEL_DIR ${ORBITER_BINARY_MODULE_DIR})

set(ORBITER_LIB ${ORBITER_BINARY_DIR}/Src/Orbiter/Orbiter.lib)
set(ORBITER_SDK_LIB ${ORBITER_BINARY_DIR}/Core/Orbitersdk/Orbitersdk.lib)
set(ORBITER_DLGCTRL_LIB ${ORBITER_BINARY_DIR}/Core/DlgCtrl/DlgCtrl.lib)

set(EXTERN_PATH "${CMAKE_SOURCE_DIR}/Extern")

set(DX7SDK_PATH "${EXTERN_PATH}/mssdk_dx7")
set(DX7SDK_INCLUDE_DIR "${DX7SDK_PATH}/include")
set(DX7SDK_LIB_DIR "${DX7SDK_PATH}/lib")

set(ZLIB_PATH "${EXTERN_PATH}/Zlib")
set(ZLIB_INCLUDE_DIR "${ZLIB_PATH}/include")
set(ZLIB_LIB_DIR "${ZLIB_PATH}/lib")
set(ZLIB_BIN_DIR "${ZLIB_PATH}/bin")
set(ZLIB_LIBRARIES "${ZLIB_LIB_DIR}/zlibstatic.lib")

set(ORBITER_TOOL_MESHC ${ORBITER_BINARY_DIR}/Utils/meshc/meshc.exe)

find_package(HTMLHelp)

if(MSVC)
  set(CMAKE_EXE_LINKER_FLAGS
      "${CMAKE_EXE_LINKER_FLAGS} /NODEFAULTLIB:\"LIBCMTD\"")
endif()

# Sub-projects
add_subdirectory(Src)
add_subdirectory(Core)
add_subdirectory(Utils)

add_custom_target(BinAssets ALL
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_SOURCE_DIR}/BinAssets/ ${CMAKE_BINARY_DIR}
)

add_custom_target(Scenarios ALL
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_SOURCE_DIR}/Scenarios/ ${CMAKE_BINARY_DIR}/Scenarios
)

add_custom_target(Textures ALL
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_SOURCE_DIR}/Textures/ ${CMAKE_BINARY_DIR}/Textures
)

add_custom_target(Meshes ALL
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_SOURCE_DIR}/Meshes/ ${CMAKE_BINARY_DIR}/Meshes
)

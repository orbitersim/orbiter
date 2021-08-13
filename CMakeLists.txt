# Licensed under the MIT License

cmake_minimum_required(VERSION 3.10)

# set the project name and version
project(D3D9Client VERSION 31.0)

# Additional compiler flags
set(Flags "/MP")

# Check build platform
if(CMAKE_SIZEOF_VOID_P EQUAL 8) # 64-bit build?
	set(BUILD64 1)
	set(ARCH "x64")
else()
	set(BUILD64 0)
	set(ARCH "x86")
endif()

# Location of DirectX SDK
# set(DXSDK_DIR "C:/Program Files (x86)/Microsoft DirectX SDK (June 2010)")

if (NOT "$ENV{DXSDK_DIR}" STREQUAL "")
	file(TO_CMAKE_PATH "$ENV{DXSDK_DIR}" DXSDK_DIR)
else()
    set(DXSDK_DIR "C:/Program Files (x86)/Microsoft DirectX SDK (June 2010)")
endif()
set(DXSDK_LIB_DIR ${DXSDK_DIR}/lib/${ARCH})

set(ORBITER_SOURCE_SDK_INCLUDE_DIR ${CMAKE_BINARY_DIR}/Orbitersdk/include)
set(ORBITER_SOURCE_SDK_LIB_DIR ${CMAKE_BINARY_DIR}/Orbitersdk/lib)

add_subdirectory(Orbitersdk/D3D9Client)
add_subdirectory(Orbitersdk/samples/DX9ExtMFD)
add_subdirectory(Orbitersdk/samples/GenericCamera)

file( COPY ${CMAKE_SOURCE_DIR}/Meshes/ DESTINATION ${CMAKE_BINARY_DIR}/Meshes )
file( COPY ${CMAKE_SOURCE_DIR}/Config/ DESTINATION ${CMAKE_BINARY_DIR}/Config )
file( COPY ${CMAKE_SOURCE_DIR}/Textures/ DESTINATION ${CMAKE_BINARY_DIR}/Textures )
file( COPY ${CMAKE_SOURCE_DIR}/Orbitersdk/doc/ DESTINATION ${CMAKE_BINARY_DIR}/Orbitersdk/doc )
file( COPY ${CMAKE_SOURCE_DIR}/Orbitersdk/include/ DESTINATION ${CMAKE_BINARY_DIR}/Orbitersdk/include )
file( COPY ${CMAKE_SOURCE_DIR}/Orbitersdk/lib/ DESTINATION ${CMAKE_BINARY_DIR}/Orbitersdk/lib )
file( COPY ${CMAKE_SOURCE_DIR}/Modules/D3D9Client/ DESTINATION ${CMAKE_BINARY_DIR}/Modules/D3D9Client )
file( COPY ${CMAKE_SOURCE_DIR}/D3D9Client.cfg DESTINATION ${CMAKE_BINARY_DIR} )
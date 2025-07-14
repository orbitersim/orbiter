project(dxmath)

Include(FetchContent)

FetchContent_Declare(
	dxmath 
	GIT_REPOSITORY https://github.com/microsoft/DirectXMath.git
	GIT_TAG apr2025
	)

FetchContent_MakeAvailable(dxmath)

message(STATUS "Using environment: DXMATH_DIR=${DXMATH_DIR}")

file(GLOB DXMATH_INL_FILES "${dxmath_SOURCE_DIR}/Inc/*.inl")
file(GLOB DXMATH_H_FILES "${dxmath_SOURCE_DIR}/Inc/*.h")
file(GLOB DXMATH_EXT_FILES "${dxmath_SOURCE_DIR}/Extensions/*.h")
file(GLOB DXMATH_SH_FILES "${dxmath_SOURCE_DIR}/SHMath/*.*")

file(COPY ${DXMATH_INL_FILES} DESTINATION ${DXMATH_DIR})
file(COPY ${DXMATH_H_FILES} DESTINATION ${DXMATH_DIR})
file(COPY ${DXMATH_EXT_FILES} DESTINATION ${DXMATH_DIR})
file(COPY ${DXMATH_SH_FILES} DESTINATION ${DXMATH_DIR})




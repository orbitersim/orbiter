# The XRSoundLib library target
add_library(XRSound_lib STATIC
	XRSound.cpp
	XRSoundImpl.cpp
)

set_target_properties(XRSound_lib
	PROPERTIES
	ARCHIVE_OUTPUT_DIRECTORY ${ORBITER_BINARY_SDK_DIR}/XRSound
	OUTPUT_NAME XRSound
)

target_include_directories(XRSound_lib PUBLIC
	${ORBITER_SOURCE_SDK_INCLUDE_DIR}
	${CMAKE_CURRENT_SOURCE_DIR}/../XRSoundDLL
)

# The debug version of XRSoundLib
add_library(XRSound_lib_DBG STATIC
	XRSound.cpp
	XRSoundImpl.cpp
)

set_target_properties(XRSound_lib_DBG
	PROPERTIES
	ARCHIVE_OUTPUT_DIRECTORY ${ORBITER_BINARY_SDK_DIR}/XRSound
	OUTPUT_NAME XRSoundD
)

target_include_directories(XRSound_lib_DBG PUBLIC
	${ORBITER_SOURCE_SDK_INCLUDE_DIR}
	${CMAKE_CURRENT_SOURCE_DIR}/../XRSoundDLL
)

set(DBG_COMPILE_FLAGS "/Zi /Ob0 /Od /MDd /RTC1")
set(DBG_LINKER_FLAGS "/debug")
set_target_properties(XRSound_lib_DBG
	PROPERTIES
	COMPILE_FLAGS ${DBG_COMPILE_FLAGS}
	LINK_FLAGS ${DBG_LINKER_FLAGS}
)

install(TARGETS XRSound_lib XRSound_lib_DBG
	ARCHIVE
	DESTINATION ${ORBITER_INSTALL_SDK_DIR}/XRSound
)
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

install(TARGETS XRSound_lib
	ARCHIVE
	DESTINATION ${ORBITER_INSTALL_SDK_DIR}/XRSound
)
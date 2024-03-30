# Licensed under the MIT License


# =============================================================================
# DX9ExtMFD
# =============================================================================

set(PRJ DX9ExtMFD)

set(Sources
	ExtMFD.cpp
	MFDWindow.cpp
)
set(Headers
	MFDWindow.h
	resource.h
)
set(Other
	ExtMFD.rc
	Bitmaps/bt_stick.bmp
)
	
source_group(APIHeaders FILES ${APIHeaders})
source_group(Headers FILES ${Headers})
source_group(Sources FILES ${Sources})
source_group(Other FILES ${Other})

add_library(${PRJ} SHARED
	${Sources}
	${Headers}
    ${APIHeaders}
	${Other}
)

target_link_libraries(${PRJ} debug ${DebugLib})
target_link_libraries(${PRJ} optimized ${ReleaseLib})

#target_include_directories(${PRJ} PUBLIC ${INCLUDE_DIR} ${IncludeDirs})

set_target_properties(${PRJ}
	PROPERTIES
	RUNTIME_OUTPUT_DIRECTORY_DEBUG ${PLUGIN_DIR}
	RUNTIME_OUTPUT_DIRECTORY_RELEASE ${PLUGIN_DIR}
	FOLDER DX9ExtMFD
)

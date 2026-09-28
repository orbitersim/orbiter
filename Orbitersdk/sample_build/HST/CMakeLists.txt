# Licensed under the MIT License

# =============================================================================
# HubbleSpaceTelescope
# =============================================================================

set(PRJ HST)

set(Sources
	HST.cpp
	HST_Lua.cpp
)

set(Headers
	HST.h
)


source_group(APIHeaders FILES ${APIHeaders})
source_group(Sources FILES ${Sources})

add_library(${PRJ} SHARED
	${Sources}
	${Headers}
    ${APIHeaders}
)

target_link_libraries(${PRJ} debug ${DebugLib})
target_link_libraries(${PRJ} optimized ${ReleaseLib})

#target_include_directories(${PRJ} PUBLIC ${INCLUDE_DIR} ${IncludeDirs})

if(WIN32) # Windows: upstream
set_target_properties(${PRJ}
	PROPERTIES
	RUNTIME_OUTPUT_DIRECTORY_DEBUG ${MODULE_DIR}
	RUNTIME_OUTPUT_DIRECTORY_RELEASE ${MODULE_DIR}
	FOLDER Vessel
)
else() # Linux port
set_target_properties(${PRJ}
	PROPERTIES
	LIBRARY_OUTPUT_DIRECTORY_DEBUG ${MODULE_DIR}
	LIBRARY_OUTPUT_DIRECTORY_RELEASE ${MODULE_DIR}
	FOLDER Vessel
)
endif() # WIN32

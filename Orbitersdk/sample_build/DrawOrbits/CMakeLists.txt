# Licensed under the MIT License

# =============================================================================
# DrawOrbits
# =============================================================================

set(PRJ DrawOrbits)

set(Sources
	Draw.cpp
	Orbit.cpp
	Reference.cpp
	Tools.cpp
)
set(Headers
	Orbit.h
	Reference.h
	Tools.h
)
if(WIN32) # Windows: upstream
set(Other
	Orbits.rc
)
else() # Linux port
set(Other
)
endif() # WIN32
	
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
if(NOT WIN32) # Linux port
orbiter_rc_resources(${PRJ} Orbits.rc)
endif() # NOT WIN32

target_link_libraries(${PRJ} debug ${DebugLib})
target_link_libraries(${PRJ} optimized ${ReleaseLib})

#target_include_directories(${PRJ} PUBLIC ${INCLUDE_DIR} ${IncludeDirs})

if(WIN32) # Windows: upstream
set_target_properties(${PRJ}
	PROPERTIES
	RUNTIME_OUTPUT_DIRECTORY_DEBUG ${PLUGIN_DIR}
	RUNTIME_OUTPUT_DIRECTORY_RELEASE ${PLUGIN_DIR}
	FOLDER Misc
)
else() # Linux port
set_target_properties(${PRJ}
	PROPERTIES
	LIBRARY_OUTPUT_DIRECTORY_DEBUG ${PLUGIN_DIR}
	LIBRARY_OUTPUT_DIRECTORY_RELEASE ${PLUGIN_DIR}
	FOLDER Misc
)
endif() # WIN32

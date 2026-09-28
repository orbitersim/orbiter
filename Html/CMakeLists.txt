add_subdirectory(Scenarios)

if(WIN32) # Windows: upstream
if(ORBITER_MAKE_DOC)
	add_subdirectory(Main)
endif()
else() # Linux port
if(ORBITER_MAKE_DOC OR ORBITER_MAKE_HELP) # not upstream: ORBITER_MAKE_HELP
	add_subdirectory(Main)
endif()
endif() # WIN32

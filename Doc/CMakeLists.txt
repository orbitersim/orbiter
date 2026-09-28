if(WIN32) # Windows: upstream
add_subdirectory("Orbiter Developer Manual")
add_subdirectory("Orbiter Technical Reference")
add_subdirectory("Orbiter User Manual")
else() # Linux port
add_subdirectory("Orbiter Developer Manual Linux")
add_subdirectory("Orbiter Technical Reference Linux")
add_subdirectory("Orbiter User Manual Linux")
endif() # WIN32

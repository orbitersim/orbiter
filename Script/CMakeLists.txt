install(
    DIRECTORY .
    DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/Script
    FILES_MATCHING
    PATTERN "*.lua"
    PATTERN "*.dat"
)

# Collect all .po files recursively
file(GLOB_RECURSE PO_FILES
    "${CMAKE_CURRENT_SOURCE_DIR}/*.po"
)

# Install them flat into i18n/
install(
    FILES ${PO_FILES}
    DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/i18n
)

generate_pot_from_files(${CMAKE_CURRENT_SOURCE_DIR}/solsys2016.pot ${CMAKE_CURRENT_SOURCE_DIR}/solsys2016.lua)
generate_pot_from_files(${CMAKE_CURRENT_SOURCE_DIR}/Challenges/Challenges.pot
	${CMAKE_CURRENT_SOURCE_DIR}/Challenges/Challenge1.lua
	${CMAKE_CURRENT_SOURCE_DIR}/Challenges/Challenge2.lua
	${CMAKE_CURRENT_SOURCE_DIR}/Challenges/Challenge3.lua
	${CMAKE_CURRENT_SOURCE_DIR}/Challenges/Challenge4.lua)

generate_pot_from_files(${CMAKE_CURRENT_SOURCE_DIR}/Demos/AtmAutopilot.pot ${CMAKE_CURRENT_SOURCE_DIR}/Demos/AtmAutopilot.lua)

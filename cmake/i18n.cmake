# Copyright (c) 2026 Gondos
# Licensed under the MIT License

function(generate_pot TARGET_NAME)
    # Use the directory where this function is called as the source
    set(SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}")

    if(ARGC GREATER 1)
        set(OUTPUT_DIR "${ARGV1}")
    else()
        set(OUTPUT_DIR "${CMAKE_CURRENT_SOURCE_DIR}/i18n")
    endif()

    file(MAKE_DIRECTORY "${OUTPUT_DIR}")

    # --- 1. File list ---
    set(FILELIST "${CMAKE_BINARY_DIR}/i18n_${TARGET_NAME}_sources.txt")
    file(WRITE "${FILELIST}" "")

    # Recursively collect all source/header files
    file(GLOB_RECURSE SOURCES
        CONFIGURE_DEPENDS
        "${SOURCE_DIR}/*.cpp"
        "${SOURCE_DIR}/*.c"
        "${SOURCE_DIR}/*.hpp"
        "${SOURCE_DIR}/*.h"
        "${SOURCE_DIR}/*.lua"
    )

    foreach(src ${SOURCES})
        file(APPEND "${FILELIST}" "${src}\n")
    endforeach()

    # --- 2. Output .pot ---
    set(POT_FILE "${OUTPUT_DIR}/${TARGET_NAME}.pot")

    # --- 3. Add custom command ---
    add_custom_command(
        OUTPUT "${POT_FILE}"
        COMMAND $<TARGET_FILE:lua::exe> "${CMAKE_SOURCE_DIR}/Utils/xgettext.lua" "--strip-prefix=${CMAKE_SOURCE_DIR}" "${POT_FILE}" "${FILELIST}"
        DEPENDS "${CMAKE_SOURCE_DIR}/Utils/xgettext.lua" ${SOURCES} "${FILELIST}"
        WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}"
        COMMENT "Extracting translations for ${TARGET_NAME}"
        VERBATIM
    )

    # --- 4. Add a custom target ---
    add_custom_target(i18n-${TARGET_NAME} ALL
        DEPENDS "${POT_FILE}"
    )
endfunction()

function(generate_pot_from_files OUTPUT_POT_FILE)
    if(ARGC LESS 2)
        message(FATAL_ERROR "generate_pot requires at least an output file and one input file")
    endif()

    # Remaining arguments are source files
    set(SOURCES ${ARGV})
    list(REMOVE_AT SOURCES 0) # remove OUTPUT_POT_FILE

    # --- 1. File list ---
    get_filename_component(FILENAME "${OUTPUT_POT_FILE}" NAME)
    set(FILELIST "${CMAKE_BINARY_DIR}/i18n_${FILENAME}_sources.txt")
    file(WRITE "${FILELIST}" "")
    foreach(src ${SOURCES})
        file(APPEND "${FILELIST}" "${src}\n")
    endforeach()


    # Ensure output directory exists
    get_filename_component(OUTPUT_DIR "${OUTPUT_POT_FILE}" DIRECTORY)
    file(MAKE_DIRECTORY "${OUTPUT_DIR}")

    add_custom_command(
        OUTPUT "${OUTPUT_POT_FILE}"
        COMMAND $<TARGET_FILE:lua::exe>
                "${CMAKE_SOURCE_DIR}/Utils/xgettext.lua"
                "--strip-prefix=${CMAKE_SOURCE_DIR}"
                "${OUTPUT_POT_FILE}"
                ${FILELIST}
        DEPENDS "${CMAKE_SOURCE_DIR}/Utils/xgettext.lua" ${SOURCES}
        COMMENT "Extracting translations -> ${OUTPUT_POT_FILE}"
        VERBATIM
    )

    # Create deterministic target name from output filename
    get_filename_component(_pot_name "${OUTPUT_POT_FILE}" NAME_WE)

    add_custom_target(i18n-${_pot_name} ALL
        DEPENDS "${OUTPUT_POT_FILE}"
    )
endfunction()

function(generate_flights_pots ROOT_DIR)
    get_filename_component(ROOT_DIR_ABS "${ROOT_DIR}" ABSOLUTE)
    message(STATUS "Searching system.dat files in ${ROOT_DIR_ABS}")

    # Find all system.dat files recursively
    file(GLOB_RECURSE SYSTEM_DAT_FILES
        CONFIGURE_DEPENDS
        "${ROOT_DIR_ABS}/system.dat"
    )

    if (NOT SYSTEM_DAT_FILES)
        message(STATUS "No system.dat files found in ${ROOT_DIR_ABS}")
        return()
    endif()

    set(ALL_POT_FILES)

    foreach(DAT_FILE ${SYSTEM_DAT_FILES})
		message(STATUS "Found ${DAT_FILE}")

        get_filename_component(DAT_DIR ${DAT_FILE} DIRECTORY)
        set(POT_FILE "${DAT_DIR}/system.pot")

        add_custom_command(
            OUTPUT ${POT_FILE}
            COMMAND $<TARGET_FILE:lua::exe>
                    ${CMAKE_SOURCE_DIR}/Utils/dat2pot.lua
                    "--strip-prefix=${CMAKE_SOURCE_DIR}"
                    ${DAT_FILE} ${POT_FILE}
            DEPENDS
                ${DAT_FILE}
                ${CMAKE_SOURCE_DIR}/Utils/dat2pot.lua
            WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
            COMMENT "Generating ${POT_FILE}"
            VERBATIM
        )

        list(APPEND ALL_POT_FILES ${POT_FILE})
    endforeach()

    add_custom_target(generate_system_pots ALL
        DEPENDS ${ALL_POT_FILES}
    )

endfunction()

# Installs .po files from a directory (default: ${CMAKE_CURRENT_SOURCE_DIR}/i18n)
#
# Usage:
#   install_po_files()                  # uses default i18n folder
#   install_po_files("${CMAKE_BINARY_DIR}/my_i18n")  # custom folder
function(install_po_files)
    # Determine input directory
    if(ARGC GREATER 0)
        set(INPUT_DIR "${ARGV0}")
    else()
        set(INPUT_DIR "${CMAKE_CURRENT_SOURCE_DIR}/i18n")
    endif()

    # Glob all .po files recursively
    file(GLOB_RECURSE PO_FILES
        "${INPUT_DIR}/*.po"
    )

    if(PO_FILES)
        foreach(PO_FILE IN LISTS PO_FILES)
            install(FILES "${PO_FILE}"
                DESTINATION "${ORBITER_INSTALL_ROOT_DIR}/i18n/"
            )
        endforeach()
    else()
        message(STATUS "No .po files found in ${INPUT_DIR}")
    endif()
endfunction()


function(generate_i18n_report)
    # Recursively collect all source/header files
    file(GLOB_RECURSE SOURCES
        CONFIGURE_DEPENDS
        "${CMAKE_SOURCE_DIR}/*.po"
        "${CMAKE_SOURCE_DIR}/*.pot"
    )

	# Exclude install dir
	file(TO_CMAKE_PATH "${CMAKE_INSTALL_PREFIX}" INSTALL_DIR_NORM)
	list(FILTER SOURCES EXCLUDE REGEX "^${INSTALL_DIR_NORM}/")
	# Exclude binary dir
	file(TO_CMAKE_PATH "${CMAKE_BINARY_DIR}" BINARY_DIR_NORM)
	list(FILTER SOURCES EXCLUDE REGEX "^${BINARY_DIR_NORM}/")

    set(FILELIST "${CMAKE_BINARY_DIR}/i18n_all_files.txt")
    file(WRITE "${FILELIST}" "")
    foreach(src ${SOURCES})
        file(APPEND "${FILELIST}" "${src}\n")
    endforeach()

    set(REPORT_FILE "${CMAKE_BINARY_DIR}/i18n_report.txt")
    add_custom_command(
        OUTPUT "${REPORT_FILE}"
        COMMAND $<TARGET_FILE:lua::exe> "${CMAKE_SOURCE_DIR}/Utils/report_i18n.lua" "--strip-prefix=${CMAKE_SOURCE_DIR}" "${CMAKE_BINARY_DIR}/i18n_all_files.txt" "${CMAKE_SOURCE_DIR}/TRANSLATIONS.md"
        DEPENDS "${CMAKE_SOURCE_DIR}/Utils/report_i18n.lua" ${SOURCES} "${FILELIST}"
        COMMENT "Creating translation report..."
        VERBATIM
    )
    add_custom_target(i18n-report ALL
        DEPENDS "${REPORT_FILE}"
    )
endfunction()

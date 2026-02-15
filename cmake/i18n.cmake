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
    )

    foreach(src ${SOURCES})
        file(APPEND "${FILELIST}" "${src}\n")
    endforeach()

    # --- 2. Output .pot ---
    set(POT_FILE "${OUTPUT_DIR}/${TARGET_NAME}.pot")

    # --- 3. Add custom command ---
    add_custom_command(
        OUTPUT "${POT_FILE}"
        COMMAND $<TARGET_FILE:lua::exe> "${CMAKE_SOURCE_DIR}/Utils/xgettext.lua" "${POT_FILE}" "${FILELIST}"
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

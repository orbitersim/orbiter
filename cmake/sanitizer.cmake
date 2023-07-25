function(enable_sanitizer SANITIZER)
  if(NOT MSVC)
    add_compile_options(-fsanitize=${SANITIZER} -fno-omit-frame-pointer)
    add_link_options(-fsanitize=${SANITIZER})
  elseif("${SANITIZER}" STREQUAL "leak") # MSVC CRT Debug Heap
    add_compile_definitions("_CRTDBG_MAP_ALLOC")
  elseif("${SANITIZER}" STREQUAL "address")
    if(CMAKE_SIZEOF_VOID_P EQUAL 8) # 64-bit build
        set(ASAN_ARCHITECTURE "x86_64")
        set(ASAN_LIBRARY_HINT_DIR $ENV{VCToolsInstallDir}/bin/Hostx64/x64)
    else()
        set(ASAN_ARCHITECTURE "i386")
        set(ASAN_LIBRARY_HINT_DIR $ENV{VCToolsInstallDir}/bin/Hostx86/x86)
    endif()

    if (${CMAKE_BUILD_TYPE} STREQUAL "Debug")
        set(ASAN_LIBRARY_NAME "clang_rt.asan_dbg_dynamic-${ASAN_ARCHITECTURE}.dll")
    else()
        set(ASAN_LIBRARY_NAME "clang_rt.asan_dynamic-${ASAN_ARCHITECTURE}.dll")
    endif()
    set(LLVM_SYMBOLIZER_NAME "llvm-symbolizer.exe")
    
    find_file (ASAN_LIBRARY_SOURCE
        NAMES ${ASAN_LIBRARY_NAME}
        REQUIRED
        HINTS ${ASAN_LIBRARY_HINT_DIR} $ENV{LIBPATH}
        DOC "Clang AddressSanitizer runtime"
    )

    find_file (LLVM_SYMBOLIZER_SOURCE
        NAMES ${LLVM_SYMBOLIZER_NAME}
        REQUIRED
        HINTS ${ASAN_LIBRARY_HINT_DIR} $ENV{LIBPATH}
        DOC "LLVM symbolizer executable"
    )

    add_custom_command(
        COMMENT "Copying ${ASAN_LIBRARY_SOURCE} to ${CMAKE_BINARY_DIR}}"
        OUTPUT ${CMAKE_BINARY_DIR}/${ASAN_LIBRARY_NAME}
        MAIN_DEPENDENCY ${ASAN_LIBRARY_SOURCE}
        COMMAND ${CMAKE_COMMAND} -E copy ${ASAN_LIBRARY_SOURCE} ${CMAKE_BINARY_DIR}
    )

    add_custom_command(
        COMMENT "Copying ${LLVM_SYMBOLIZER_SOURCE} to ${CMAKE_BINARY_DIR}}"
        OUTPUT ${CMAKE_BINARY_DIR}/${LLVM_SYMBOLIZER_NAME}
        MAIN_DEPENDENCY ${LLVM_SYMBOLIZER_SOURCE}
        COMMAND ${CMAKE_COMMAND} -E copy ${LLVM_SYMBOLIZER_SOURCE} ${CMAKE_BINARY_DIR}
    )

    add_custom_target(CopyAsanBinaries ALL DEPENDS ${ASAN_LIBRARY_NAME} ${LLVM_SYMBOLIZER_NAME})
    add_compile_options(/fsanitize=${SANITIZER} /Zi /Oy-)
  else()
    message(FATAL_ERROR "MSVC does not support sanitizer ${SANITIZER}")
  endif()
  message(STATUS "Enabled sanitize=${SANITIZER}")
endfunction()
## irrKlang
if(NOT IRRKLANG_DIR)
	set(IRRKLANG_DIR ${EXTERN_DIR}/irrKlang/${ARCH})

	if(NOT EXISTS ${IRRKLANG_DIR})
		if(BUILD64)
			set(IRRKLANG_URL https://www.ambiera.at/downloads/irrKlang-64bit-1.6.0.zip)
		else()
			set(IRRKLANG_URL https://www.ambiera.at/downloads/irrKlang-32bit-1.6.0.zip)
		endif()

		include(FetchContent)
		FetchContent_Declare(irrKlang
			URL ${IRRKLANG_URL}
			SOURCE_DIR ${IRRKLANG_DIR}
			DOWNLOAD_EXTRACT_TIMESTAMP ON
		)

		message("Downloading irrKlang from ${IRRKLANG_URL} ...")
		FetchContent_MakeAvailable(irrKlang)

	else()
		message("Found existing irrKlang at ${IRRKLANG_DIR}")
	endif()

elseif(NOT EXISTS ${IRRKLANG_DIR})
	message(FATAL_ERROR "IRRKLANG_DIR does not exist")
endif()

if(BUILD64)
	set(IRRKLANG_BIN_DIR ${IRRKLANG_DIR}/bin/winx64-visualStudio)
	set(IRRKLANG_LIB_DIR ${IRRKLANG_DIR}/lib/Winx64-visualStudio)
else()
	# because of a bug in the 32bit version of irrKlang (it has hidden "__MACOSX" dir),
	# CMake does not strip the top-level directory
	string(APPEND IRRKLANG_DIR /irrKlang-1.6.0)

	set(IRRKLANG_BIN_DIR ${IRRKLANG_DIR}/bin/win32-visualStudio)
	set(IRRKLANG_LIB_DIR ${IRRKLANG_DIR}/lib/Win32-visualStudio)
endif()
set(IRRKLANG_LIB ${IRRKLANG_LIB_DIR}/irrKlang.lib)

## XRSound
set(ASSET_DIR ${CMAKE_CURRENT_SOURCE_DIR}/assets)
set(SOLUTION_DIR ${CMAKE_CURRENT_SOURCE_DIR}/src)
set(IRRKLANG_DLL_DIR ${IRRKLANG_DIR})
# Deploy assets
file(GLOB config_files ${ASSET_DIR}/XRSound/*.cfg)

add_custom_target(XRSound_assets ALL
	COMMAND ${CMAKE_COMMAND} -E make_directory ${ORBITER_BINARY_ROOT_DIR}/Doc
	COMMAND ${CMAKE_COMMAND} -E make_directory ${ORBITER_BINARY_ROOT_DIR}/XRSound
	COMMAND ${CMAKE_COMMAND} -E make_directory ${ORBITER_BINARY_SDK_DIR}/XRSound/
	COMMAND ${CMAKE_COMMAND} -E copy ${config_files} ${ORBITER_BINARY_ROOT_DIR}/XRSound/
	COMMAND ${CMAKE_COMMAND} -E copy ${ASSET_DIR}/XRSound/ReadMe.txt ${ORBITER_BINARY_ROOT_DIR}/XRSound/
	COMMAND ${CMAKE_COMMAND} -E copy "${ASSET_DIR}/Doc/XRSound User Manual.pdf" ${ORBITER_BINARY_ROOT_DIR}/Doc/
	COMMAND ${CMAKE_COMMAND} -E copy ${SOLUTION_DIR}/XRSound.h ${ORBITER_BINARY_SDK_DIR}/XRSound/
	COMMAND ${CMAKE_COMMAND} -E copy ${IRRKLANG_BIN_DIR}/ikpMP3.dll ${ORBITER_BINARY_ROOT_DIR}
	COMMAND ${CMAKE_COMMAND} -E copy ${IRRKLANG_BIN_DIR}/ikpFlac.dll ${ORBITER_BINARY_ROOT_DIR}
	COMMAND ${CMAKE_COMMAND} -E copy ${IRRKLANG_BIN_DIR}/irrKlang.dll ${ORBITER_BINARY_ROOT_DIR}
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${ASSET_DIR}/XRSound/Default ${ORBITER_BINARY_ROOT_DIR}/XRSound/Default
)

install(DIRECTORY ${ORBITER_BINARY_ROOT_DIR}/XRSound
	DESTINATION ${ORBITER_INSTALL_ROOT_DIR}
)
install(DIRECTORY ${ORBITER_BINARY_SDK_DIR}/XRSound
	DESTINATION ${ORBITER_INSTALL_SDK_DIR}
)
install(DIRECTORY ${ORBITER_BINARY_ROOT_DIR}/XRSound/Default
	DESTINATION ${ORBITER_BINARY_ROOT_DIR}/XRSound
)
install(FILES "${ORBITER_BINARY_ROOT_DIR}/Doc/XRSound User Manual.pdf"
	DESTINATION ${ORBITER_INSTALL_DOC_DIR}/
)
install(FILES ${ORBITER_BINARY_ROOT_DIR}/ikpMP3.dll ${ORBITER_BINARY_ROOT_DIR}/ikpFlac.dll ${ORBITER_BINARY_ROOT_DIR}/irrKlang.dll
	DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/
)

add_subdirectory(src)

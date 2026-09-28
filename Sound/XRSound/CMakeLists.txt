## irrKlang
if(WIN32) # Windows: upstream
if(NOT IRRKLANG_DIR)
	set(IRRKLANG_DIR ${EXTERN_DIR}/irrKlang/${ARCH})

	if(NOT EXISTS ${IRRKLANG_DIR})
		if(BUILD64)
			set(IRRKLANG_URL https://web.archive.org/web/20250213015920/https://www.ambiera.at/downloads/irrKlang-64bit-1.6.0.zip)
		else()
			set(IRRKLANG_URL https://web.archive.org/web/20250225180248/http://www.ambiera.at/downloads/irrKlang-32bit-1.6.0.zip)
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
else() # Linux port
# irrKlang left out: a closed-source Windows library; XRSound plays through its own mixer on PipeWire (src/Audio)
# not upstream: the mixer's decoders, public-domain single-file libraries (OGG stb_vorbis, MP3 dr_mp3, FLAC dr_flac), pinned
include(FetchContent)
FetchContent_Declare(stb_vorbis
	URL https://raw.githubusercontent.com/nothings/stb/2c980bb59875b0d32144a71867fbdebb2f77cd20/stb_vorbis.c
	URL_HASH SHA256=4c7cb2ff1f7011e9d67950446b7eb9ca044f2e464d76bfbb0b84dd2e23e65636
	DOWNLOAD_NO_EXTRACT TRUE
)
FetchContent_Declare(dr_mp3
	URL https://raw.githubusercontent.com/mackron/dr_libs/5690d4671d7ad07ae6021756d7222eb159745f06/dr_mp3.h
	URL_HASH SHA256=d80e6800ddde29d50988ce894affe601858802d1bb8f047f98b84b33ae66c11a
	DOWNLOAD_NO_EXTRACT TRUE
)
FetchContent_Declare(dr_flac
	URL https://raw.githubusercontent.com/mackron/dr_libs/69d777c482775858e8ea8a7b047c9bcd451febc8/dr_flac.h
	URL_HASH SHA256=a35468f278f17cf9538ce189debf76a5854a77e139980f7b5185d67566a0c837
	DOWNLOAD_NO_EXTRACT TRUE
)
# not upstream: tracker modules (MOD, S3M, XM, IT, which irrKlang played) through libxmp-lite (MIT), built static
FetchContent_Declare(libxmp_lite
	URL https://github.com/libxmp/libxmp/releases/download/libxmp-4.6.3/libxmp-lite-4.6.3.tar.gz
	URL_HASH SHA256=fa6465d8b911363ae602c7baaa625ca1f5223142d10bb4682029c4d2f630cb62
	DOWNLOAD_EXTRACT_TIMESTAMP TRUE
)
set(CMAKE_POLICY_DEFAULT_CMP0077 NEW) # its options take the values below
set(BUILD_STATIC ON)
set(BUILD_SHARED OFF)
set(LIBXMP_PIC ON)
FetchContent_MakeAvailable(stb_vorbis dr_mp3 dr_flac libxmp_lite)
set(AUDIO_DECODER_INCLUDE_DIRS ${stb_vorbis_SOURCE_DIR} ${dr_mp3_SOURCE_DIR} ${dr_flac_SOURCE_DIR})
set(AUDIO_DECODER_LIBS libxmp-lite::xmp_lite_static)
endif() # WIN32

## XRSound
set(ASSET_DIR ${CMAKE_CURRENT_SOURCE_DIR}/assets)
set(SOLUTION_DIR ${CMAKE_CURRENT_SOURCE_DIR}/src)
if(WIN32) # Windows: upstream
set(IRRKLANG_DLL_DIR ${IRRKLANG_DIR})
else() # Linux port
# IRRKLANG_DLL_DIR left out: there are no irrKlang DLLs to deploy
endif() # WIN32
# Deploy assets
file(GLOB config_files ${ASSET_DIR}/XRSound/*.cfg)

if(WIN32) # Windows: upstream
add_custom_target(XRSound_assets ALL
	COMMAND ${CMAKE_COMMAND} -E make_directory ${ORBITER_BINARY_ROOT_DIR}/Doc
	COMMAND ${CMAKE_COMMAND} -E make_directory ${ORBITER_BINARY_ROOT_DIR}/XRSound
	COMMAND ${CMAKE_COMMAND} -E make_directory ${ORBITER_BINARY_SDK_DIR}/XRSound/
	COMMAND ${CMAKE_COMMAND} -E copy ${config_files} ${ORBITER_BINARY_ROOT_DIR}/XRSound/
	COMMAND ${CMAKE_COMMAND} -E copy ${ASSET_DIR}/XRSound/ReadMe.txt ${ORBITER_BINARY_ROOT_DIR}/XRSound/
	COMMAND ${CMAKE_COMMAND} -E copy "${ASSET_DIR}/Doc/XRSound User Manual.pdf" ${ORBITER_BINARY_ROOT_DIR}/Doc/
	COMMAND ${CMAKE_COMMAND} -E copy_if_different ${SOLUTION_DIR}/XRSound.h ${ORBITER_BINARY_SDK_DIR}/XRSound/
	COMMAND ${CMAKE_COMMAND} -E copy ${IRRKLANG_BIN_DIR}/ikpMP3.dll ${ORBITER_BINARY_ROOT_DIR}
	COMMAND ${CMAKE_COMMAND} -E copy ${IRRKLANG_BIN_DIR}/ikpFlac.dll ${ORBITER_BINARY_ROOT_DIR}
	COMMAND ${CMAKE_COMMAND} -E copy ${IRRKLANG_BIN_DIR}/irrKlang.dll ${ORBITER_BINARY_ROOT_DIR}
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${ASSET_DIR}/XRSound/Default ${ORBITER_BINARY_ROOT_DIR}/XRSound/Default
)
else() # Linux port
add_custom_target(XRSound_assets ALL
	COMMAND ${CMAKE_COMMAND} -E make_directory ${ORBITER_BINARY_ROOT_DIR}/Doc
	COMMAND ${CMAKE_COMMAND} -E make_directory ${ORBITER_BINARY_ROOT_DIR}/XRSound
	COMMAND ${CMAKE_COMMAND} -E make_directory ${ORBITER_BINARY_SDK_DIR}/XRSound/
	COMMAND ${CMAKE_COMMAND} -E copy ${config_files} ${ORBITER_BINARY_ROOT_DIR}/XRSound/
	COMMAND ${CMAKE_COMMAND} -E copy ${ASSET_DIR}/XRSound/ReadMe.txt ${ORBITER_BINARY_ROOT_DIR}/XRSound/
	COMMAND ${CMAKE_COMMAND} -E copy "${ASSET_DIR}/Doc/XRSound User Manual.pdf" ${ORBITER_BINARY_ROOT_DIR}/Doc/
	COMMAND ${CMAKE_COMMAND} -E copy_if_different ${SOLUTION_DIR}/XRSound.h ${ORBITER_BINARY_SDK_DIR}/XRSound/
	# ikpMP3.dll, ikpFlac.dll and irrKlang.dll left out: the mixer and its decoders are built into XRSound.so
	# copy_directory_if_different: the 200 MB of default sounds are not copied again on every build
	COMMAND ${CMAKE_COMMAND} -E copy_directory_if_different ${ASSET_DIR}/XRSound/Default ${ORBITER_BINARY_ROOT_DIR}/XRSound/Default
)
endif() # WIN32

install(DIRECTORY ${ORBITER_BINARY_ROOT_DIR}/XRSound
	DESTINATION ${ORBITER_INSTALL_ROOT_DIR}
)
install(DIRECTORY ${ORBITER_BINARY_SDK_DIR}/XRSound
	DESTINATION ${ORBITER_INSTALL_SDK_DIR}
)
install(DIRECTORY ${ORBITER_BINARY_ROOT_DIR}/XRSound/Default
	DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/XRSound
)
install(FILES "${ORBITER_BINARY_ROOT_DIR}/Doc/XRSound User Manual.pdf"
	DESTINATION ${ORBITER_INSTALL_DOC_DIR}/
)
if(WIN32) # Windows: upstream
install(FILES ${ORBITER_BINARY_ROOT_DIR}/ikpMP3.dll ${ORBITER_BINARY_ROOT_DIR}/ikpFlac.dll ${ORBITER_BINARY_ROOT_DIR}/irrKlang.dll
	DESTINATION ${ORBITER_INSTALL_ROOT_DIR}/
)
else() # Linux port
# irrKlang DLL install left out: no irrKlang
endif() # WIN32

add_subdirectory(src)
if(NOT WIN32) # Linux port

# not upstream: mixer test, decodes a WAV from the assets and plays 0.5 s (reports it when no PipeWire daemon answers)
if(ORBITER_MAKE_TESTS)
	add_executable(XRSound.AudioEngine.Test
		test/AudioEngineTest.cpp
		src/Audio/AudioEngine.cpp
		src/Audio/AudioDecode.cpp
	)
	set_target_properties(XRSound.AudioEngine.Test PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${ORBITER_BINARY_ROOT_DIR}/Tests)
	target_include_directories(XRSound.AudioEngine.Test PRIVATE ${SOLUTION_DIR} ${AUDIO_DECODER_INCLUDE_DIRS})
	target_compile_definitions(XRSound.AudioEngine.Test PRIVATE XRSOUND_TEST_SOUNDS="${ASSET_DIR}/XRSound/Default")
	target_link_libraries(XRSound.AudioEngine.Test PkgConfig::PIPEWIRE ${AUDIO_DECODER_LIBS})
	add_test(NAME XRSound.AudioEngine.Test COMMAND $<TARGET_FILE:XRSound.AudioEngine.Test>)
	set_tests_properties(XRSound.AudioEngine.Test PROPERTIES TIMEOUT 20)
endif()
endif() # NOT WIN32

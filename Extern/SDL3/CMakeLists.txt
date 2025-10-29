project(SDL3)

Include(FetchContent)

set(SDL_SHARED ON)
set(SDL_STATIC OFF)
#set(SDL_INSTALL ON)


FetchContent_Declare(
        SDL3
        GIT_REPOSITORY https://github.com/libsdl-org/SDL.git
#        GIT_TAG release-3.2.24
		GIT_TAG 8824eace93de228032639f11df262df91a6257a4
		OVERRIDE_FIND_PACKAGE
)
FetchContent_MakeAvailable(SDL3)

set_target_properties(SDL3-shared PROPERTIES
        ARCHIVE_OUTPUT_DIRECTORY ${ORBITER_BINARY_ROOT_DIR}
        LIBRARY_OUTPUT_DIRECTORY ${ORBITER_BINARY_ROOT_DIR}
        RUNTIME_OUTPUT_DIRECTORY ${ORBITER_BINARY_ROOT_DIR}
)

set_target_properties(SDL3_Headers PROPERTIES
        ARCHIVE_OUTPUT_DIRECTORY "${ORBITER_INSTALL_SDK_DIR}/include"
        LIBRARY_OUTPUT_DIRECTORY "${ORBITER_INSTALL_SDK_DIR}/include"
        RUNTIME_OUTPUT_DIRECTORY "${ORBITER_INSTALL_SDK_DIR}/include"
)

file(GLOB SDL3_HEADERS "${SDL3_SOURCE_DIR}/include/SDL3/*.h" "${SDL3_BINARY_DIR}/include-revision/SDL3/*.h")
file(COPY ${SDL3_HEADERS} DESTINATION "${ORBITER_BINARY_SDK_DIR}/include/SDL3")

install(TARGETS SDL3_Headers RUNTIME DESTINATION "${ORBITER_INSTALL_SDK_DIR}/include")
install(TARGETS SDL3-shared RUNTIME DESTINATION ${ORBITER_INSTALL_ROOT_DIR})

###############
## SDL_mixer ##
###############

set(BUILD_SHARED_LIBS ON)

FetchContent_Declare(
        SDL3_mixer
        GIT_REPOSITORY https://github.com/libsdl-org/SDL_mixer.git
        GIT_TAG 5cdf029bae982df1d6c210f915fc151a616d982f
)
FetchContent_MakeAvailable(SDL3_mixer)

install(TARGETS SDL3_mixer-shared RUNTIME DESTINATION ${ORBITER_INSTALL_ROOT_DIR})
file(GLOB SDL3_MIXER_HEADERS "${SDL3_mixer_SOURCE_DIR}/include/SDL3_mixer/*.h")
file(COPY ${SDL3_MIXER_HEADERS} DESTINATION "${ORBITER_BINARY_SDK_DIR}/include/SDL3_Mixer")

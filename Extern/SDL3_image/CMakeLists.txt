project(SDL3_image)

Include(FetchContent)

# These formats we need
set(SDLIMAGE_BMP ON)
set(SDLIMAGE_JPG ON)
set(SDLIMAGE_PNG ON)
set(SDLIMAGE_GIF ON)
# Disable auto-enabled formats that we don't need (which add some additional dependencies)
set(SDLIMAGE_AVIF OFF)
set(SDLIMAGE_LBM OFF)
set(SDLIMAGE_PCX OFF)
set(SDLIMAGE_PNM OFF)
set(SDLIMAGE_QOI OFF)
set(SDLIMAGE_SVG OFF)
set(SDLIMAGE_TGA OFF)
set(SDLIMAGE_TIF OFF)
set(SDLIMAGE_WEBP OFF)
set(SDLIMAGE_XCF OFF)
set(SDLIMAGE_XPM OFF)
set(SDLIMAGE_XV OFF)

FetchContent_Declare(
        SDL3_image
        GIT_REPOSITORY https://github.com/libsdl-org/SDL_Image.git
        GIT_TAG release-3.2.0
)
FetchContent_MakeAvailable(SDL3_image)

set_target_properties(SDL3_image-shared PROPERTIES
        ARCHIVE_OUTPUT_DIRECTORY ${ORBITER_BINARY_ROOT_DIR}
        LIBRARY_OUTPUT_DIRECTORY ${ORBITER_BINARY_ROOT_DIR}
        RUNTIME_OUTPUT_DIRECTORY ${ORBITER_BINARY_ROOT_DIR}
)
file(GLOB SDL3_image_HEADERS "${SDL3_image_SOURCE_DIR}/include/SDL3_image/*.h")
file(COPY ${SDL3_image_HEADERS} DESTINATION "${ORBITER_BINARY_SDK_DIR}/include/SDL3_image")

install(TARGETS SDL3_image-shared RUNTIME DESTINATION ${ORBITER_INSTALL_ROOT_DIR})
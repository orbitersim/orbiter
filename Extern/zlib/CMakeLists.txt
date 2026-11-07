find_package(ZLIB REQUIRED)
add_library(zlib INTERFACE)
target_link_libraries(zlib INTERFACE ZLIB::ZLIB)

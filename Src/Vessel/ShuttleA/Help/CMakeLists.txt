set(srcdir ${CMAKE_CURRENT_SOURCE_DIR}/..)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/config.ld.in ${CMAKE_CURRENT_BINARY_DIR}/config.ld)
orbiter_help_pages(ShuttleA Vessels LDOC)

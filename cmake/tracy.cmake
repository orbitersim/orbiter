# Copyright (c) Gondos
# Licensed under the MIT License

# Tracy profiler integration (https://github.com/wolfpld/tracy)
#
# The profiler is split in 2 parts:
#    - client code that needs to be added to the process to profile
#    - server process that handles aggregating and displaying the profiling data.
#
# Since Orbiter uses multiple DLLs, we must use a shared library for the client code.
# This TracyClient library must be added as dependency to DLLs wanting to use the profiler.
#
# When profiling is disabled, we don't build the server nor the client library.
# However we still need the Tracy.hpp file in the include path (tracy macros will be nop-ed).
#
# We build the server ourselves to make sure the version is compatible with the client.
#
# The default behavior makes the installation step copy files in bin/include/lib/cmake directories.
# We don't want that so we use EXCLUDE_FROM_ALL and then manually install the files where needed.
#
# The profiler is meant for the Orbiter core and possibly for in tree modules, but not external plugins,
# so we don't add the dependencies in the Orbiter SDK.
#
# HOW TO USE:
# - in the Orbiter core:
#   just #include "Tracy.hpp" and use the ZoneScoped macro on the functions you want to profile
# - in Orbiter modules:
#   add tracy to your include path:
#       target_include_directories(...
#       	PUBLIC ${ORBITER_SOURCE_SDK_INCLUDE_DIR}
#       	...
#       	PUBLIC ${TRACY_CLIENT_INCLUDE}
#       )
#   add the tracy client library: 
#       target_link_libraries(...
#       	${ORBITER_LIB}
#       	${ORBITER_SDK_LIB}
#       	...
#       	${TRACY_CLIENT}
#       )
#
# - build the solution
# - start Orbiter
# - start the profiler (in Utils) and connect to the Orbiter process
# - have fun


if(ORBITER_TRACY_PROFILER)
	set(TRACY_ENABLE ON)
else()
	set(TRACY_ENABLE OFF)
endif()

set(BUILD_SHARED_LIBS ON)
set(TRACY_ONLY_LOCALHOST ON)
set(TRACY_ON_DEMAND ON)
set(TRACY_ONLY_IPV4 ON)

Include(FetchContent)
include(ExternalProject)

FetchContent_Declare(tracy
	GIT_REPOSITORY https://github.com/wolfpld/tracy.git
	GIT_TAG v0.12.2
	GIT_SHALLOW TRUE
	EXCLUDE_FROM_ALL # prevents installation of lib and include directories in ${ORBITER_INSTALL_ROOT_DIR}
)
FetchContent_MakeAvailable(tracy)

set(TRACY_CLIENT_INCLUDE ${tracy_SOURCE_DIR}/public/tracy CACHE PATH "Tracy public include path")

# Build the server and client library only if profiling is enabled.
if(ORBITER_TRACY_PROFILER)
	# To be used in CMakeLists.txt. 
	set(TRACY_CLIENT TracyClient)

	# Copy the DLL alongside the main Orbiter binary.
	install(TARGETS TracyClient RUNTIME DESTINATION ${ORBITER_INSTALL_ROOT_DIR})

	# The root CMakeLists.txt file of the repo only handles the client side.
	# The server is inside the profiler subdirectory.
	ExternalProject_Add(profiler
		SOURCE_DIR "${tracy_SOURCE_DIR}/profiler"
		INSTALL_DIR ${CMAKE_INSTALL_PREFIX}/Orbiter/Utils
		CMAKE_ARGS -DCMAKE_INSTALL_PREFIX=<INSTALL_DIR>
               -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE} -DCMAKE_INSTALL_BINDIR=.
	)
endif()

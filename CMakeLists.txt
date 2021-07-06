cmake_minimum_required(VERSION 3.10)

# Set the project name
project (Orbiter VERSION 2021.1)

# Define directories and file paths for some common binaries
set(OVP_DIR "${CMAKE_SOURCE_DIR}/OVP")
set(GDICLIENT_DIR "${OVP_DIR}/GDIClient")

set(ORBITER_SOURCE_DIR ${CMAKE_SOURCE_DIR}/Src/Orbiter)
set(ORBITER_SOURCE_SDK_DIR ${CMAKE_SOURCE_DIR}/Orbitersdk)
set(ORBITER_SOURCE_SDK_INCLUDE_DIR ${ORBITER_SOURCE_SDK_DIR}/include)
set(ORBITER_SOURCE_MODULE_DIR ${ORBITER_SOURCE_SDK_DIR}/Module) # this will probably be rebased to Src
set(ORBITER_BINARY_DIR ${CMAKE_BINARY_DIR})
set(ORBITER_BINARY_MODULE_DIR ${ORBITER_BINARY_DIR}/Modules)
set(ORBITER_BINARY_CELBODY_DIR ${ORBITER_BINARY_MODULE_DIR}/Celbody)
set(ORBITER_BINARY_VESSEL_DIR ${ORBITER_BINARY_MODULE_DIR})


set(ORBITER_LIB $<TARGET_LINKER_FILE:Orbiter>)
set(ORBITER_SDK_LIB $<TARGET_FILE:Orbitersdk>)
set(ORBITER_DLGCTRL_LIB $<TARGET_FILE:DlgCtrl>)

set(EXTERN_PATH "${CMAKE_SOURCE_DIR}/Extern")

set(DX7SDK_PATH "${EXTERN_PATH}/mssdk_dx7")
set(DX7SDK_INCLUDE_DIR "${DX7SDK_PATH}/include")
set(DX7SDK_LIB_DIR "${DX7SDK_PATH}/lib")

set(ZLIB_PATH "${EXTERN_PATH}/Zlib")
set(ZLIB_INCLUDE_DIR "${ZLIB_PATH}/include")
set(ZLIB_LIB_DIR "${ZLIB_PATH}/lib")
set(ZLIB_BIN_DIR "${ZLIB_PATH}/bin")
set(ZLIB_LIBRARIES "${ZLIB_LIB_DIR}/zlibstatic.lib")

set(ORBITER_TOOL_MESHC ${ORBITER_BINARY_DIR}/Utils/meshc/meshc.exe)

find_package(HTMLHelp)

if(MSVC)
  set(CMAKE_EXE_LINKER_FLAGS
      "${CMAKE_EXE_LINKER_FLAGS} /NODEFAULTLIB:\"LIBCMTD\"")
endif()

add_custom_target(BinAssets ALL
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_SOURCE_DIR}/BinAssets/ ${CMAKE_BINARY_DIR}
)

add_custom_target(Scenarios ALL
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_SOURCE_DIR}/Scenarios/ ${CMAKE_BINARY_DIR}/Scenarios
)

add_custom_target(Textures ALL
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_SOURCE_DIR}/Textures/ ${CMAKE_BINARY_DIR}/Textures
)

add_custom_target(Meshes ALL
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_SOURCE_DIR}/Meshes/ ${CMAKE_BINARY_DIR}/Meshes
)

option(ORBITER_MAKE_DOC
	"Create documentation"
	ON
)

if (ORBITER_MAKE_DOC)

find_program(ORBITER_ODT_TO_PDF_COMPILER
	soffice
	HINTS "/Program Files/LibreOffice/program"
)

set(ORBITER_ODT_TO_PDF_FLAGS
	"--headless --convert-to pdf --outdir <outdir> <infile>"
	CACHE
	STRING
	"Compiler options for the OdtToPdfCompiler"
)

endif()

function(odt_to_pdf_arglist infile arglist source_path target_path)
	string(REPLACE
		"<outdir>" "${CMAKE_CURRENT_BINARY_DIR}"
		tmp1_string
		${ORBITER_ODT_TO_PDF_FLAGS}
	)
	string(REPLACE
		"<infile>" "${CMAKE_CURRENT_SOURCE_DIR}/${infile}.odt"
		tmp2_string
		${tmp1_string}
	)
	separate_arguments(odt_arg
		WINDOWS_COMMAND
		${tmp2_string}
	)
	set(${arglist} ${odt_arg} PARENT_SCOPE)
	set(${source_path} "${CMAKE_CURRENT_SOURCE_DIR}/${infile}.odt" PARENT_SCOPE)
	set(${target_path} "${CMAKE_CURRENT_BINARY_DIR}/${infile}.pdf" PARENT_SCOPE)
endfunction()


# Sub-projects
add_subdirectory(Src)
add_subdirectory(Core)
add_subdirectory(Utils)
add_subdirectory(Doc)

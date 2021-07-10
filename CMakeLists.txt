cmake_minimum_required(VERSION 3.15)

# Set the project name
project (Orbiter VERSION 2021.1)

# Set the pools for compilers that can't run concurrently
set_property(GLOBAL PROPERTY JOB_POOLS latex=1 soffice=1)

# Group projects in VS
set_property(GLOBAL PROPERTY USE_FOLDERS ON)

# Define directories and file paths for some common binaries
set(OVP_DIR "${CMAKE_SOURCE_DIR}/OVP")
set(GDICLIENT_DIR "${OVP_DIR}/GDIClient")

set(ORBITER_SOURCE_ROOT_DIR ${CMAKE_SOURCE_DIR})
set(ORBITER_SOURCE_DIR ${ORBITER_SOURCE_ROOT_DIR}/Src/Orbiter)
set(ORBITER_SOURCE_MODULE_DIR ${ORBITER_SOURCE_ROOT_DIR}/Src/Module)
set(ORBITER_SOURCE_SDK_DIR ${ORBITER_SOURCE_ROOT_DIR}/Orbitersdk)
set(ORBITER_SOURCE_SDK_INCLUDE_DIR ${ORBITER_SOURCE_SDK_DIR}/include)
set(ORBITER_BINARY_ROOT_DIR ${CMAKE_BINARY_DIR})
set(ORBITER_BINARY_MODULE_DIR ${ORBITER_BINARY_ROOT_DIR}/Modules)
set(ORBITER_BINARY_CELBODY_DIR ${ORBITER_BINARY_MODULE_DIR}/Celbody)
set(ORBITER_BINARY_PLUGIN_DIR ${ORBITER_BINARY_MODULE_DIR}/Plugin)
set(ORBITER_BINARY_VESSEL_DIR ${ORBITER_BINARY_MODULE_DIR})
set(ORBITER_BINARY_TEXTURES_DIR ${ORBITER_BINARY_ROOT_DIR}/Textures)

set(ORBITER_LIB $<TARGET_LINKER_FILE:Orbiter>)
set(ORBITER_SDK_LIB $<TARGET_FILE:Orbitersdk>)
set(ORBITER_DLGCTRL_LIB $<TARGET_FILE:DlgCtrl>)

set(EXTERN_DIR "${CMAKE_SOURCE_DIR}/Extern")

set(DX7SDK_PATH "${EXTERN_DIR}/mssdk_dx7")
set(DX7SDK_INCLUDE_DIR "${DX7SDK_PATH}/include")
set(DX7SDK_LIB_DIR "${DX7SDK_PATH}/lib")

set(ZLIB_PATH "${EXTERN_DIR}/Zlib")
set(ZLIB_INCLUDE_DIR "${ZLIB_PATH}/include")
set(ZLIB_LIB_DIR "${ZLIB_PATH}/lib")
set(ZLIB_BIN_DIR "${ZLIB_PATH}/bin")
set(ZLIB_LIBRARIES "${ZLIB_LIB_DIR}/zlibstatic.lib")

set(LUA_DIR "${EXTERN_DIR}/Lua")
set(LUA_INCLUDE_DIR "${LUA_DIR}/include")
set(LUA_LIB_DIR "${LUA_DIR}/lib")
set(LUA_DLL_DIR "${LUA_DIR}/dll")
set(LUA_LIB "${LUA_DLL_DIR}/lua5.1.lib")

#set(ORBITER_TOOL_MESHC ${ORBITER_BINARY_ROOT_DIR}/Utils/meshc/meshc.exe)
set(ORBITER_TOOL_MESHC "$<TARGET_FILE:meshc>")


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
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_SOURCE_DIR}/Textures/ ${ORBITER_BINARY_TEXTURES_DIR}
)

add_custom_target(Meshes ALL
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_SOURCE_DIR}/Meshes/ ${CMAKE_BINARY_DIR}/Meshes
)

add_custom_target(Script ALL
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_SOURCE_DIR}/Script/ ${CMAKE_BINARY_DIR}/Script
)

add_custom_target(Config ALL
	COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_SOURCE_DIR}/Config/ ${CMAKE_BINARY_DIR}/Config
)

set_target_properties(BinAssets Scenarios Textures Meshes Script Config
	PROPERTIES
	FOLDER Data
)

# =====================================================================================
# Create symbolic links for externally installed planet textures

set(ORBITER_TEXTURE_INSTALL_DIR
	${ORBITER_BINARY_ROOT_DIR}/Textures
	CACHE
	PATH
	"Texture installation directory (e.g. C:/Orbiter2016/Textures)"
)
string(COMPARE NOTEQUAL ${ORBITER_TEXTURE_INSTALL_DIR} "${ORBITER_BINARY_ROOT_DIR}/Textures" CUSTOM_TEXTURE_DIR)

if (${CUSTOM_TEXTURE_DIR})
	string(REPLACE  "/" "\\" ORBITER_TEXTURE_INSTALL_DIR_W "${ORBITER_TEXTURE_INSTALL_DIR}")
	configure_file(${ORBITER_SOURCE_DIR}/Orbiter.cfg.in ${ORBITER_BINARY_ROOT_DIR}/Orbiter.cfg)
endif()


# =====================================================================================
# Find required and optional build tools

option(ORBITER_MAKE_DOC
	"Create documentation"
	ON
)

if (ORBITER_MAKE_DOC)

	find_program(ODT_TO_PDF_COMPILER
		soffice
		HINTS "/Program Files/LibreOffice/program"
	)

	set(ODT_TO_PDF_FLAGS
		"--headless --convert-to pdf --outdir <outdir> <infile>"
		CACHE
		STRING
		"Compiler options for the OdtToPdfCompiler"
	)

	option(ORBITER_MAKE_TECHNOTES
		"Create Orbiter Technical Notes"
		ON
	)

	if(ORBITER_MAKE_TECHNOTES)
	
		find_package(LATEX)
		
	endif()

endif()

function(odt_to_pdf_arglist infile arglist source_path target_path)
	string(REPLACE
		"<outdir>" "${CMAKE_CURRENT_BINARY_DIR}"
		tmp1_string
		${ODT_TO_PDF_FLAGS}
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

if(ORBITER_MAKE_DOC)
	add_subdirectory(Doc)
endif()

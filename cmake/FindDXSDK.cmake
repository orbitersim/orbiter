find_path(DXSDK_DIR
	Utilities/bin/dx_setenv.cmd
	HINTS "C:/Program Files (x86)/Microsoft DirectX SDK (June 2010)"
	PATHS ENV DXSDK_DIR
)

if(${DXSDK_DIR} STREQUAL "DXSDK_DIR-NOTFOUND")
	set(DXSDK_FOUND FALSE)
else()
	set(DXSDK_FOUND TRUE)
endif()

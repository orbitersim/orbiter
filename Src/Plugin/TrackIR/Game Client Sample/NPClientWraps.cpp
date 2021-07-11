// *******************************************************************************
// *
// * Module Name:
// *   NPClientWraps.cpp
// *
// * Software Engineer:
// *   Doyle Nickless - GoFlight Inc., for Eye Control Technology.
// *
// * Abstract:
// *   This module implements the wrapper code for interfacing to the NaturalPoint
// *   Game Client API.  Developers of client apps can include this module into
// *   their projects to simplify communication with the NaturalPoint software.
// *
// *   This is necessary since the NPClient DLL is run-time linked rather than
// *   load-time linked, avoiding the need to link a static library into the
// *   client program (only this module is needed, and can be supplied in source
// *   form.)
// *
// * Environment:
// *   User mode
// *
// *******************************************************************************
//
#include "stdafx.h"
#include "string.h"

#include "NPClient.h"
#include "NPClientWraps.h"

/////////////
// Defines ///////////////////////////////////////////////////////////////////////
/////////////
//

/////////////////
// Global Data ///////////////////////////////////////////////////////////////////
/////////////////
//
PF_NP_REGISTERWINDOWHANDLE       gpfNP_RegisterWindowHandle = NULL;
PF_NP_UNREGISTERWINDOWHANDLE     gpfNP_UnregisterWindowHandle = NULL;
PF_NP_REGISTERPROGRAMPROFILEID   gpfNP_RegisterProgramProfileID = NULL;
PF_NP_QUERYVERSION               gpfNP_QueryVersion = NULL;
PF_NP_REQUESTDATA                gpfNP_RequestData = NULL;
PF_NP_GETSIGNATURE               gpfNP_GetSignature = NULL;
PF_NP_GETDATA                    gpfNP_GetData = NULL;
PF_NP_STARTCURSOR                gpfNP_StartCursor = NULL;
PF_NP_STOPCURSOR                 gpfNP_StopCursor = NULL;
PF_NP_RECENTER	                 gpfNP_ReCenter = NULL;
PF_NP_STARTDATATRANSMISSION      gpfNP_StartDataTransmission = NULL;
PF_NP_STOPDATATRANSMISSION       gpfNP_StopDataTransmission = NULL;

HMODULE ghNPClientDLL = (HMODULE)NULL;

////////////////////////////////////////////////////
// NaturalPoint Game Client API function wrappers /////////////////////////////
////////////////////////////////////////////////////
//
NPRESULT __stdcall NP_RegisterWindowHandle( HWND hWnd  )
{
	NPRESULT result = NP_ERR_DLL_NOT_FOUND;

	if( NULL != gpfNP_RegisterWindowHandle )
		result = (*gpfNP_RegisterWindowHandle)( hWnd );

	return result;
} // NP_RegisterWindowHandle()


NPRESULT __stdcall NP_UnregisterWindowHandle()
{
	NPRESULT result = NP_ERR_DLL_NOT_FOUND;

	if( NULL != gpfNP_UnregisterWindowHandle )
		result = (*gpfNP_UnregisterWindowHandle)();

	return result;
} // NP_UnregisterWindowHandle()


NPRESULT __stdcall NP_RegisterProgramProfileID( unsigned short wPPID )
{
	NPRESULT result = NP_ERR_DLL_NOT_FOUND;

	if( NULL != gpfNP_RegisterProgramProfileID )
		result = (*gpfNP_RegisterProgramProfileID)( wPPID );

	return result;
} // NP_RegisterProgramProfileID()


NPRESULT __stdcall NP_QueryVersion( unsigned short* pwVersion )
{
	NPRESULT result = NP_ERR_DLL_NOT_FOUND;

	if( NULL != gpfNP_QueryVersion )
		result = (*gpfNP_QueryVersion)( pwVersion );

	return result;
} // NP_QueryVersion()


NPRESULT __stdcall NP_RequestData( unsigned short wDataReq )
{
	NPRESULT result = NP_ERR_DLL_NOT_FOUND;

	if( NULL != gpfNP_RequestData )
		result = (*gpfNP_RequestData)( wDataReq );

	return result;
} // NP_RequestData()

NPRESULT __stdcall NP_GetSignature( LPTRACKIRSIGNATURE pSignature )
{
	NPRESULT result = NP_ERR_DLL_NOT_FOUND;

	if( NULL != gpfNP_GetSignature )
		result = (*gpfNP_GetSignature)( pSignature );

	return result;
} // NP_GetSignature()


NPRESULT __stdcall NP_GetData( LPTRACKIRDATA pTID )
{
	NPRESULT result = NP_ERR_DLL_NOT_FOUND;

	if( NULL != gpfNP_GetData )
		result = (*gpfNP_GetData)( pTID );

	return result;
} // NP_GetData()


NPRESULT __stdcall NP_StartCursor()
{
	NPRESULT result = NP_ERR_DLL_NOT_FOUND;

	if( NULL != gpfNP_StartCursor )
		result = (*gpfNP_StartCursor)();

	return result;
} // NP_StartCursor()


NPRESULT __stdcall NP_StopCursor()
{
	NPRESULT result = NP_ERR_DLL_NOT_FOUND;

	if( NULL != gpfNP_StopCursor )
		result = (*gpfNP_StopCursor)();

	return result;
} // NP_StopCursor()


NPRESULT __stdcall NP_ReCenter()
{
	NPRESULT result = NP_ERR_DLL_NOT_FOUND;

	if( NULL != gpfNP_ReCenter )
		result = (*gpfNP_ReCenter)();

	return result;
} // NP_ReCenter()


NPRESULT __stdcall NP_StartDataTransmission()
{
	NPRESULT result = NP_ERR_DLL_NOT_FOUND;

	if( NULL != gpfNP_StartDataTransmission )
		result = (*gpfNP_StartDataTransmission)();

	return result;
} // NP_StartDataTransmission()


NPRESULT __stdcall NP_StopDataTransmission()
{
	NPRESULT result = NP_ERR_DLL_NOT_FOUND;

	if( NULL != gpfNP_StopDataTransmission )
		result = (*gpfNP_StopDataTransmission)();

	return result;
} // NP_StopDataTransmission()


//////////////////////////////////////////////////////////////////////////////
// NPClientInit() -- Loads the DLL and retrieves pointers to all exports
//
NPRESULT NPClient_Init( LPTSTR pszDLLPath )
{
	NPRESULT result = NP_OK;
    TCHAR szFullPath[MAX_PATH * 2];
    
    if (pszDLLPath == NULL)
        return NP_ERR_DLL_NOT_FOUND;
    
    strcpy(szFullPath, pszDLLPath);

    if (lstrlen(szFullPath) > 0)        
	{
	   strcat(szFullPath, "\\");
	}
	
	strcat(szFullPath, "NPClient.dll");

	ghNPClientDLL = ::LoadLibrary( szFullPath );
	if( NULL != ghNPClientDLL )
		{
			// verify the dll signature
			gpfNP_GetSignature             = (PF_NP_GETSIGNATURE)::GetProcAddress( ghNPClientDLL, "NP_GetSignature" );

			SIGNATUREDATA pSignature;
			SIGNATUREDATA verifySignature;
			// init the signatures
			strcpy(verifySignature.DllSignature, "precise head tracking\n put your head into the game\n now go look around\n\n Copyright EyeControl Technologies");
			strcpy(verifySignature.AppSignature, "hardware camera\n software processing data\n track user movement\n\n Copyright EyeControl Technologies");
			// query the dll and compare the results
			NPRESULT vresult = NP_GetSignature( &pSignature );
			if( vresult == NP_OK )
			{
				if ((strcmp(verifySignature.DllSignature,pSignature.DllSignature)==0) 
					&& (strcmp(verifySignature.AppSignature,pSignature.AppSignature)==0))
				{	
					result = NP_OK;	

					// Get addresses of all exported functions
					gpfNP_RegisterWindowHandle     = (PF_NP_REGISTERWINDOWHANDLE)::GetProcAddress( ghNPClientDLL, "NP_RegisterWindowHandle" );
					gpfNP_UnregisterWindowHandle   = (PF_NP_UNREGISTERWINDOWHANDLE)::GetProcAddress( ghNPClientDLL, "NP_UnregisterWindowHandle" );
					gpfNP_RegisterProgramProfileID = (PF_NP_REGISTERPROGRAMPROFILEID)::GetProcAddress( ghNPClientDLL, "NP_RegisterProgramProfileID" );
					gpfNP_QueryVersion             = (PF_NP_QUERYVERSION)::GetProcAddress( ghNPClientDLL, "NP_QueryVersion" );
					gpfNP_RequestData              = (PF_NP_REQUESTDATA)::GetProcAddress( ghNPClientDLL, "NP_RequestData" );
					gpfNP_GetData                  = (PF_NP_GETDATA)::GetProcAddress( ghNPClientDLL, "NP_GetData" );
					gpfNP_StartCursor              = (PF_NP_STARTCURSOR)::GetProcAddress( ghNPClientDLL, "NP_StartCursor" );
					gpfNP_StopCursor               = (PF_NP_STOPCURSOR)::GetProcAddress( ghNPClientDLL, "NP_StopCursor" );
					gpfNP_ReCenter	               = (PF_NP_RECENTER)::GetProcAddress( ghNPClientDLL, "NP_ReCenter" );
					gpfNP_StartDataTransmission    = (PF_NP_STARTDATATRANSMISSION)::GetProcAddress( ghNPClientDLL, "NP_StartDataTransmission" );
					gpfNP_StopDataTransmission     = (PF_NP_STOPDATATRANSMISSION)::GetProcAddress( ghNPClientDLL, "NP_StopDataTransmission" );
				}
				else
				{	
					result = NP_ERR_DLL_NOT_FOUND;	
				}
			}
			else
			{
				result = NP_ERR_DLL_NOT_FOUND;	
			}
		}
		else
		result = NP_ERR_DLL_NOT_FOUND;

	return result;

} // NPClient_Init()

//////////////////////////////////////////////////////////////////////////////





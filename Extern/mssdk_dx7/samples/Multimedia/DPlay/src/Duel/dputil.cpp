//-----------------------------------------------------------------------------
// File: DPUtil.cpp
//
// Desc: Communication routines
//
// Copyright (C) 1995-1999 Microsoft Corporation. All Rights Reserved.
//-----------------------------------------------------------------------------
#include "duel.h"
#include "DPUtil.h"
#include "lobby.h"


//-----------------------------------------------------------------------------
// Globals
//-----------------------------------------------------------------------------
extern GUID           g_AppGUID;        // Duel's guid
extern DPLCONNECTION* g_pDPLConnection;  // Connection settings
extern BOOL           g_bUseProtocol;    // DirectPlay Protocol messaging
extern BOOL           g_bAsyncSupported; // Asynchronous sends supported

DPSESSIONDESC2*       g_pdpsd;           // Durrent session description
LPDIRECTPLAY4         g_pDP = NULL;      // DPlay object pointer


                                     
                                         
//-----------------------------------------------------------------------------
// Name: CheckCaps()
// Desc: Helper function to check for certain Capabilities
//-----------------------------------------------------------------------------
VOID CheckCaps()
{
    HRESULT hr;
    DPCAPS  dpcaps;
    ZeroMemory( &dpcaps, sizeof(DPCAPS) );
    dpcaps.dwSize = sizeof(DPCAPS);

    if( NULL == g_pDP )
        return;
    
    // The caps we are checking do not differ for guaranteed msg
    hr = g_pDP->GetCaps( &dpcaps, 0 );
    if( FAILED(hr) )
        return;

    // Determine if Aync messages are supported.
    g_bAsyncSupported = (dpcaps.dwFlags & DPCAPS_ASYNCSUPPORTED) != 0;

    // Diagnostic traces of caps supported
    if( g_bAsyncSupported )
    {
        TRACE(_T("Capabilities supported: Async %s %s %s\n"),
                 (dpcaps.dwFlags & DPCAPS_SENDPRIORITYSUPPORTED ? _T("SendPriority") : _T("")),
                 (dpcaps.dwFlags & DPCAPS_SENDTIMEOUTSUPPORTED ? _T("SendTimeout") : _T("")),
                 (dpcaps.dwFlags & DPCAPS_ASYNCCANCELSUPPORTED
                    ? _T("AsyncCancel") 
                    : (dpcaps.dwFlags & DPCAPS_ASYNCCANCELALLSUPPORTED
                        ? _T("AsyncCancelAll") : _T("")))
                );
    }
    else
        TRACE(_T("CheckCaps - Async not supported\n"));
}




//-----------------------------------------------------------------------------
// Name: DPUtil_FreeDirectPlay()
// Desc: Wrapper for DirectPlay Close API
//-----------------------------------------------------------------------------
HRESULT DPUtil_FreeDirectPlay()
{
    if( NULL == g_pDP )
        return E_FAIL;

    return g_pDP->Close();
}




//-----------------------------------------------------------------------------
// Name: DPUtil_InitDirectPlay()
// Desc: Wrapper for DirectPlay Create API. Retrieves a DirectPlay4/4A
//       interface based on the UNICODE flag
//-----------------------------------------------------------------------------
HRESULT DPUtil_InitDirectPlay( VOID* pCon )
{
    HRESULT hr;

    // Create a DirectPlay4(A) interface
#ifdef UNICODE
    hr = CoCreateInstance( CLSID_DirectPlay, NULL, CLSCTX_INPROC_SERVER,
                           IID_IDirectPlay4, (VOID**)&g_pDP );
#else
    hr = CoCreateInstance( CLSID_DirectPlay, NULL, CLSCTX_INPROC_SERVER,
                           IID_IDirectPlay4A, (VOID**)&g_pDP );
#endif
    if( FAILED(hr) )
        return hr;

    // Initialize w/address
    if( pCon )
    {
        hr = g_pDP->InitializeConnection( pCon, 0 );
        if( FAILED(hr) )
        {
            g_pDP->Release();
            g_pDP = NULL;
            return hr;
        }
    }

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: DPUtil_CreatePlayer()
// Desc: Wrapper for DirectPlay CreatePlayer API. 
//-----------------------------------------------------------------------------
HRESULT DPUtil_CreatePlayer( DPID* ppidID, TCHAR* strPlayerName, HANDLE hEvent, 
                             VOID* pData, DWORD dwDataSize )
{
    if( NULL == g_pDP )
        return E_FAIL;

    DPNAME dpname;
    ZeroMemory( &dpname, sizeof(DPNAME) );
    dpname.dwSize = sizeof(DPNAME);

#ifdef UNICODE
    dpname.lpszShortName  = strPlayerName;
#else
    dpname.lpszShortNameA = strPlayerName;
#endif

    return g_pDP->CreatePlayer( ppidID, &dpname, hEvent, pData, dwDataSize, 0 );
}




//-----------------------------------------------------------------------------
// Name: DPUtil_CreateSession()
// Desc: Wrapper for DirectPlay CreateSession API.Uses the global application
//       guid.
//-----------------------------------------------------------------------------
HRESULT DPUtil_CreateSession( TCHAR* strSessionName )
{
    if( NULL == g_pDP )
        return DPERR_NOINTERFACE;

    DPSESSIONDESC2 dpDesc;
    ZeroMemory( &dpDesc, sizeof(dpDesc) );
    dpDesc.dwSize  = sizeof(dpDesc);
    dpDesc.dwFlags = DPSESSION_MIGRATEHOST | DPSESSION_KEEPALIVE;
    if( g_bUseProtocol )
        dpDesc.dwFlags |= DPSESSION_DIRECTPLAYPROTOCOL;

#ifdef UNICODE
    dpDesc.lpszSessionName  = strSessionName;
#else
    dpDesc.lpszSessionNameA = strSessionName;
#endif

    // Set the application guid
    dpDesc.guidApplication = g_AppGUID;

    HRESULT hr = g_pDP->Open( &dpDesc, DPOPEN_CREATE );

    // Check for Async message support
    if( SUCCEEDED(hr) )
        CheckCaps();

    return hr;
}




//-----------------------------------------------------------------------------
// Name: DPUtil_DestroyPlayer()
// Desc: Wrapper for DirectPlay DestroyPlayer API. 
//-----------------------------------------------------------------------------
HRESULT DPUtil_DestroyPlayer( DPID pid )
{
    if( NULL == g_pDP )
        return E_FAIL;

    return g_pDP->DestroyPlayer( pid );
}




//-----------------------------------------------------------------------------
// Name: DPUtil_EnumPlayers()
// Desc: Wrapper for DirectPlay API EnumPlayers
//-----------------------------------------------------------------------------
HRESULT DPUtil_EnumPlayers( GUID* pSessionGuid,
                          LPDPENUMPLAYERSCALLBACK2 pEnumCallback, 
                          VOID* pContext, DWORD dwFlags )
{
    if( NULL == g_pDP )
        return E_FAIL;

    return g_pDP->EnumPlayers( pSessionGuid, pEnumCallback, pContext, dwFlags );
}




//-----------------------------------------------------------------------------
// Name: DPUtil_EnumSessions()
// Desc: Wrapper for DirectPlay EnumSessions API.
//-----------------------------------------------------------------------------
HRESULT DPUtil_EnumSessions( DWORD dwTimeout,
                           LPDPENUMSESSIONSCALLBACK2 pEnumCallback, 
                           VOID* pContext, DWORD dwFlags )
{
    if( NULL == g_pDP )
        return E_FAIL;

    DPSESSIONDESC2 dpDesc;
    ZeroMemory( &dpDesc, sizeof(dpDesc) );
    dpDesc.dwSize = sizeof(dpDesc);
    dpDesc.guidApplication = g_AppGUID;

    return g_pDP->EnumSessions( &dpDesc, dwTimeout, pEnumCallback,
                                pContext, dwFlags );
}




//-----------------------------------------------------------------------------
// Name: DPUtil_GetPlayerLocalData()
// Desc: Wrapper for DirectPlay GetPlayerData API.
//-----------------------------------------------------------------------------
HRESULT DPUtil_GetPlayerLocalData( DPID pid, VOID* pData, DWORD* pdwDataSize )
{
    if( NULL == g_pDP )
        return E_FAIL;

    HRESULT hr = g_pDP->GetPlayerData( pid, pData, pdwDataSize, DPGET_LOCAL );
    if( FAILED(hr) )
        TRACE( TEXT("Get Player local data failed for id %d\n"), pid );

    return hr;
}




//-----------------------------------------------------------------------------
// Name: DPUtil_GetSessionDesc()
// Desc: Wrapper for DirectPlay GetSessionDesc API. 
//-----------------------------------------------------------------------------
HRESULT DPUtil_GetSessionDesc()
{
    DWORD   dwSize;
    HRESULT hr;

    // Free old session desc, if any
    if( g_pdpsd )
        free( g_pdpsd );
    g_pdpsd = NULL;

    if( NULL == g_pDP )
        return E_FAIL;

    // First get the size for the session desc
    hr = g_pDP->GetSessionDesc( NULL, &dwSize );
    if( DPERR_BUFFERTOOSMALL == hr )
    {
        // Allocate memory for it
        g_pdpsd = (DPSESSIONDESC2*)malloc( dwSize );
        if( NULL == g_pdpsd )
            return E_OUTOFMEMORY;

        // Now get the session desc
        hr = g_pDP->GetSessionDesc( g_pdpsd, &dwSize );
    }

    return hr;
}




//-----------------------------------------------------------------------------
// Name: DPUtil_IsDPlayInitialized()
// Desc: Returns TRUE if a DirectPlay interface exists, otherwise FALSE.
//-----------------------------------------------------------------------------
BOOL DPUtil_IsDPlayInitialized()
{
    return( g_pDP ? TRUE : FALSE );
}




//-----------------------------------------------------------------------------
// Name: DPUtil_OpenSession()
// Desc: Wrapper for DirectPlay OpenSession API. 
//-----------------------------------------------------------------------------
HRESULT DPUtil_OpenSession( GUID* pSessionGUID )
{
    if( NULL == g_pDP)
        return DPERR_NOINTERFACE;

    DPSESSIONDESC2 dpDesc;
    ZeroMemory( &dpDesc, sizeof(dpDesc) );
    dpDesc.dwSize = sizeof(dpDesc);
    if( g_bUseProtocol )
        dpDesc.dwFlags = DPSESSION_DIRECTPLAYPROTOCOL;

    // Set the session guid
    if( pSessionGUID )
        dpDesc.guidInstance = (*pSessionGUID);
    // Set the application guid
    dpDesc.guidApplication = g_AppGUID;

    // Open it
    HRESULT hr = g_pDP->Open( &dpDesc, DPOPEN_JOIN );

    // Check for Async message support
    if( SUCCEEDED(hr) )
        CheckCaps();

    return hr;
}




//-----------------------------------------------------------------------------
// Name: DPUtil_Receive()
// Desc: Wrapper for DirectPlay Receive API
//-----------------------------------------------------------------------------
HRESULT DPUtil_Receive( DPID* pidFrom, DPID* pidTo, DWORD dwFlags, VOID* pData,
                      DWORD* pdwDataSize )
{
    if( NULL == g_pDP )
        return E_FAIL;

    return g_pDP->Receive( pidFrom, pidTo, dwFlags, pData, pdwDataSize );
}




//-----------------------------------------------------------------------------
// Name: DPUtil_Release()
// Desc: Wrapper for DirectPlay Release API.
//-----------------------------------------------------------------------------
HRESULT DPUtil_Release()
{
    if( NULL == g_pDP )
        return E_FAIL;

    // Free session desc, if any
    if( g_pdpsd ) 
        free( g_pdpsd );
    g_pdpsd = NULL;

    // Free connection settings structure, if any (lobby stuff)
    if( g_pDPLConnection )
        delete[] g_pDPLConnection;
    g_pDPLConnection = NULL;

    // Release dplay
    HRESULT hr = g_pDP->Release();
    g_pDP = NULL;

    return hr;
}




//-----------------------------------------------------------------------------
// Name: DPUtil_Send()
// Desc: Wrapper for DirectPlay Send[Ex] API.
//-----------------------------------------------------------------------------
HRESULT DPUtil_Send( DPID idFrom, DPID idTo, DWORD dwFlags, VOID* pData,
                   DWORD dwDataSize )
{
    if( NULL == g_pDP )
        return DPERR_NOINTERFACE;

    if (dwFlags & DPSEND_ASYNC)
        // We don't specify a priority or timeout.  Would have to check
        // GetCaps() first to see if they were supported
        return g_pDP->SendEx( idFrom, idTo, dwFlags, pData, dwDataSize,
                              0, 0, NULL, NULL );
    else
        return g_pDP->Send( idFrom, idTo, dwFlags, pData, dwDataSize );
}




//-----------------------------------------------------------------------------
// Name: DPUtil_SetPlayerLocalData()
// Desc: Wrapper for DirectPlay SetPlayerData API
//-----------------------------------------------------------------------------
HRESULT DPUtil_SetPlayerLocalData( DPID pid, VOID* pData, DWORD dwSize )
{
    if( NULL == g_pDP )
        return E_FAIL;

    HRESULT hr = g_pDP->SetPlayerData( pid, pData, dwSize, DPSET_LOCAL );
    if( FAILED(hr) )
        TRACE( TEXT("Set Player local data failed for id %d\n"), pid );
    
    return hr;
}




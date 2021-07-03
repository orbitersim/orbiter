//-----------------------------------------------------------------------------
// File: DPUtil.h
//
// Desc: Communication routines include file
//
// Copyright (C) 1995-1999 Microsoft Corporation. All Rights Reserved.
//-----------------------------------------------------------------------------
#define IDIRECTPLAY2_OR_GREATER
#include <dplay.h>


//-----------------------------------------------------------------------------
// Prototypes
//-----------------------------------------------------------------------------
HRESULT DPUtil_FreeDirectPlay();
HRESULT DPUtil_InitDirectPlay( VOID* pCon );
HRESULT DPUtil_CreatePlayer( DPID* ppidID, LPTSTR pPlayerName, HANDLE hEvent, 
                           VOID* pData, DWORD dwDataSize );
HRESULT DPUtil_CreateSession( TCHAR* strSessionName );
HRESULT DPUtil_DestroyPlayer( DPID pid );
HRESULT DPUtil_EnumPlayers( GUID* pSessionGuid,
                          LPDPENUMPLAYERSCALLBACK2 lpEnumCallback, 
                          VOID* pContext, DWORD dwFlags );
HRESULT DPUtil_EnumSessions( DWORD dwTimeout,
                           LPDPENUMSESSIONSCALLBACK2 lpEnumCallback, 
                           VOID* pContext, DWORD dwFlags );
HRESULT DPUtil_GetSessionDesc();
BOOL    DPUtil_IsDPlayInitialized();
HRESULT DPUtil_OpenSession( GUID* pSessionGuid );
HRESULT DPUtil_Receive( DPID* pidFrom, DPID* pidTo, DWORD dwFlags, VOID* pData, 
                      DWORD* pdwDataSize );
HRESULT DPUtil_Release();
HRESULT DPUtil_Send( DPID idFrom, DPID idTo, DWORD dwFlags, VOID* pData, 
                   DWORD dwDataSize );
HRESULT DPUtil_SetPlayerLocalData( DPID pid, VOID* pData, DWORD dwSize );
HRESULT DPUtil_GetPlayerLocalData( DPID pid, VOID* pData, DWORD* pdwDataSize );




 

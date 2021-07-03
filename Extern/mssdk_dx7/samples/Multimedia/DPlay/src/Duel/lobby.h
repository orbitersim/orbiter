//-----------------------------------------------------------------------------
// File: Lobby.h
//
// Desc: DP lobby related routines include file
//
// Copyright (C) 1995-1999 Microsoft Corporation. All Rights Reserved.
//-----------------------------------------------------------------------------
#define IDIRECTPLAY2_OR_GREATER
#include <dplobby.h>


//-----------------------------------------------------------------------------
// LobbyMessageReceive Modes
//-----------------------------------------------------------------------------
#define LMR_PROPERTIES          0
#define LMR_CONNECTIONSETTINGS  1




//-----------------------------------------------------------------------------
// Prototypes
//-----------------------------------------------------------------------------
HRESULT DPLobbyRelease();
HRESULT DPLobbySetConnectionSettings();

BOOL    DoingLobbyMessages();
HRESULT LobbyMessageInit();
HRESULT LobbyMessageReceive( DWORD dwMode );
HRESULT LobbyMessageSetProperty( const GUID* pPropTagGUID, VOID* pData,
                                 DWORD dwDataSize );



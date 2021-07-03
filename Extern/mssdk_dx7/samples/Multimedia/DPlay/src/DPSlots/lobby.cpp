//-----------------------------------------------------------------------------
// File: Lobby.cpp
//
// Desc: Uses information from the lobby to establish a connection.
//
// Copyright (C) 1996-1999 Microsoft Corporation.  All Rights Reserved.
//-----------------------------------------------------------------------------
#include <windows.h>
#include <windowsx.h>
#include <dplobby.h>
#include "dpslots.h"




//-----------------------------------------------------------------------------
// Name: ConnectUsingLobby()
// Desc:
//-----------------------------------------------------------------------------
HRESULT ConnectUsingLobby( DPLAYINFO* pDPInfo )
{
    LPDIRECTPLAY4A      pDPlay   = NULL;
    LPDIRECTPLAYLOBBY3A pDPLobby = NULL;
    DPLCONNECTION*      pConnectionSettings = NULL;
    DPID                dpidPlayer;
    DWORD               dwSize;
    BOOL                bIsHost;
    DWORD               dwPlayerFlags;
    HRESULT             hr;

    // Get an ANSI DirectPlay lobby interface
    hr = CoCreateInstance( CLSID_DirectPlayLobby, NULL, CLSCTX_INPROC_SERVER, 
                           IID_IDirectPlayLobby3A, (VOID**)&pDPLobby );
    if( FAILED(hr) )
        goto FAILURE;

    // Get connection settings from the lobby
    // If this routine returns DPERR_NOTLOBBIED, then a lobby did not
    // launch this application and the user needs to configure the connection.

    // Pass in a NULL pointer to just get the size of the connection setttings
    hr = pDPLobby->GetConnectionSettings( 0, NULL, &dwSize );
    if( DPERR_BUFFERTOOSMALL != hr )
        goto FAILURE;

    // Allocate memory for the connection setttings
    pConnectionSettings = (DPLCONNECTION*)GlobalAllocPtr( GHND, dwSize );
    if( NULL == pConnectionSettings )
    {
        hr = DPERR_OUTOFMEMORY;
        goto FAILURE;
    }

    // Get the connection settings
    hr = pDPLobby->GetConnectionSettings( 0, pConnectionSettings, &dwSize );
    if( FAILED(hr) )
        goto FAILURE;

    // See if we are the host or not
    if( pConnectionSettings->dwFlags & DPLCONNECTION_CREATESESSION )
    {
        pConnectionSettings->lpSessionDesc->dwFlags = NONSECURESESSIONFLAGS;
        bIsHost = TRUE;
    }
    else
    {
        bIsHost = FALSE;
    }

    // Store the updated connection settings
    hr = pDPLobby->SetConnectionSettings( 0, 0, pConnectionSettings );
    if( FAILED(hr) )
        goto FAILURE;

    // Connect to the session - getting an ANSI IDirectPlay4A interface
    hr = pDPLobby->ConnectEx( 0, IID_IDirectPlay4A, (VOID**)&pDPlay, NULL );
    if( FAILED(hr) )
        goto FAILURE;

    // If we are hosting then we need to create a server player
    if( bIsHost )
        dwPlayerFlags = SERVERPLAYERFLAGS;
    else
        dwPlayerFlags = CLIENTPLAYERFLAGS;

    // Create a player with the name returned in the connection settings
    hr = pDPlay->CreatePlayer( &dpidPlayer,
                               pConnectionSettings->lpPlayerName, 
                               pDPInfo->hPlayerEvent, NULL, 0, dwPlayerFlags );
    if( FAILED(hr) )
        goto FAILURE;

    // Return connection info
    pDPInfo->pDPlay     = pDPlay;
    pDPInfo->dpidPlayer = dpidPlayer;
    pDPInfo->bIsHost    = bIsHost;
    pDPInfo->bIsSecure  = FALSE;

    pDPlay = NULL;  // set to NULL here so it won't release below

FAILURE:
    if( pDPlay )
        pDPlay->Release();

    if( pDPLobby )
        pDPLobby->Release();

    if( pConnectionSettings )
        GlobalFreePtr( pConnectionSettings );

    return hr;
}



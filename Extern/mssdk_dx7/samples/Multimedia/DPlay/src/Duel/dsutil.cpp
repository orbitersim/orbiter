//-----------------------------------------------------------------------------
// File: dsutil.cpp
//
// Desc: Routines for dealing with sounds from resources
//
// Copyright (C) 1995-1999 Microsoft Corporation. All Rights Reserved.
//-----------------------------------------------------------------------------
#include <windows.h>
#include <mmsystem.h>
#include <dsound.h>
#include "dsutil.h"


//-----------------------------------------------------------------------------
// Globals
//-----------------------------------------------------------------------------
LPDIRECTSOUND g_pDS = NULL;




//-----------------------------------------------------------------------------
// Name: DSUtil_LoadSoundBuffer()
// Desc:
//-----------------------------------------------------------------------------
LPDIRECTSOUNDBUFFER DSUtil_LoadSoundBuffer( LPDIRECTSOUND pDS, LPCTSTR strName )
{
    LPDIRECTSOUNDBUFFER pDSB = NULL;
    DSBUFFERDESC        dsbd;
    BYTE*               pbWaveData;

    ZeroMemory( &dsbd, sizeof(dsbd) );
    dsbd.dwSize  = sizeof(dsbd);
    dsbd.dwFlags = DSBCAPS_STATIC|DSBCAPS_CTRLPAN|DSBCAPS_CTRLVOLUME|
                   DSBCAPS_CTRLFREQUENCY|DSBCAPS_CTRLPOSITIONNOTIFY;

    if( SUCCEEDED( DSUtil_GetWaveResource( NULL, strName, &dsbd.lpwfxFormat,
                                           &pbWaveData, &dsbd.dwBufferBytes ) ) )
    {

        if( SUCCEEDED( pDS->CreateSoundBuffer( &dsbd, &pDSB, NULL ) ) )
        {
            if( FAILED( DSUtil_FillSoundBuffer( pDSB, pbWaveData,
                                                dsbd.dwBufferBytes ) ) )
            {
                pDSB->Release();
                pDSB = NULL;
            }
        }
        else
        {
            pDSB = NULL;
        }
    }

    return pDSB;
}




//-----------------------------------------------------------------------------
// Name: DSUtil_ReloadSoundBuffer()
// Desc: 
//-----------------------------------------------------------------------------
HRESULT DSUtil_ReloadSoundBuffer( LPDIRECTSOUNDBUFFER pDSB, LPCTSTR strName )
{
    BYTE* pbWaveData;
    DWORD cbWaveSize;

    if( FAILED( DSUtil_GetWaveResource( NULL, strName, NULL, &pbWaveData,
                                        &cbWaveSize ) ) )
        return E_FAIL;

    if( FAILED( pDSB->Restore() ) )
        return E_FAIL;

    if( FAILED( DSUtil_FillSoundBuffer( pDSB, pbWaveData, cbWaveSize ) ) )
        return E_FAIL;

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: 
// Desc: 
//-----------------------------------------------------------------------------
HRESULT DSUtil_GetWaveResource( HMODULE hModule, LPCTSTR strName,
                                WAVEFORMATEX** ppWaveHeader, BYTE** ppbWaveData,
                                DWORD* pcbWaveSize )
{
    HRSRC   hResInfo;
    HGLOBAL hResData;
    VOID*   pvRes;

    if( NULL == ( hResInfo = FindResource( hModule, strName, TEXT("WAVE") ) ) )
    {
        if( NULL == ( hResInfo = FindResource( hModule, strName, TEXT("WAV") ) ) )
            return E_FAIL;
    }

    if( NULL == ( hResData = LoadResource( hModule, hResInfo ) ) )
        return E_FAIL;

    if( NULL == ( pvRes = LockResource( hResData ) ) )
        return E_FAIL;

    if( FAILED( DSUtil_ParseWaveResource( pvRes, ppWaveHeader, ppbWaveData,
                                          pcbWaveSize ) ) )
        return E_FAIL;

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: 
// Desc: 
//-----------------------------------------------------------------------------
SoundObject* DSUtil_CreateSound( LPCTSTR strName, DWORD dwNumConcurrentBuffers )
{
    SoundObject*   pSound = NULL;
    LPWAVEFORMATEX pWaveHeader;
    BYTE*          pbData;
    DWORD          cbData;

    if( NULL == g_pDS )
        return NULL;

    if( dwNumConcurrentBuffers < 1 )
        dwNumConcurrentBuffers = 1;

    if( SUCCEEDED( DSUtil_GetWaveResource( NULL, strName, &pWaveHeader,
                                           &pbData, &cbData ) ) )
    {
        pSound = new SoundObject;
        pSound->dwNumBuffers = dwNumConcurrentBuffers;
        pSound->pbWaveData   = pbData;
        pSound->cbWaveSize   = cbData;
        pSound->dwCurrent    = 0;
        pSound->pdsbBuffers  = new LPDIRECTSOUNDBUFFER[dwNumConcurrentBuffers+1];

        pSound->pdsbBuffers[0] = DSUtil_LoadSoundBuffer( g_pDS, strName );

        for( DWORD i=1; i<pSound->dwNumBuffers; i++ )
        {
            if( FAILED( g_pDS->DuplicateSoundBuffer( pSound->pdsbBuffers[0],
                                                   &pSound->pdsbBuffers[i] ) ) )
            {
                pSound->pdsbBuffers[i] = DSUtil_LoadSoundBuffer( g_pDS, strName );
                if( NULL == pSound->pdsbBuffers[i] )
                {
                    DSUtil_DestroySound( pSound );
                    pSound = NULL;
                    break;
                }
            }
        }
    }

    return pSound;
}




//-----------------------------------------------------------------------------
// Name: DSUtil_DestroySound()
// Desc: 
//-----------------------------------------------------------------------------
VOID DSUtil_DestroySound( SoundObject* pSound )
{
    if( pSound )
    {
        for( DWORD i=0; i<pSound->dwNumBuffers; i++ )
        {
            if( pSound->pdsbBuffers[i] )
                pSound->pdsbBuffers[i]->Release();
        }
        
        delete pSound->pdsbBuffers;
        delete pSound;
    }
}




//-----------------------------------------------------------------------------
// Name: 
// Desc: 
//-----------------------------------------------------------------------------
LPDIRECTSOUNDBUFFER DSUtil_GetFreeSoundBuffer( SoundObject* pSound )
{
    HRESULT hr;
    DWORD   dwStatus;

    if( NULL == pSound )
        return NULL;

    LPDIRECTSOUNDBUFFER pDSB = pSound->pdsbBuffers[pSound->dwCurrent];
    if( NULL == pDSB )
        return NULL;

    hr = pDSB->GetStatus( &dwStatus );
    if( FAILED(hr) )
        dwStatus = 0;

    if( dwStatus & DSBSTATUS_PLAYING )
    {
        if( pSound->dwNumBuffers <= 1 )
            return NULL;

        if( ++pSound->dwCurrent >= pSound->dwNumBuffers )
            pSound->dwCurrent = 0;

        pDSB = pSound->pdsbBuffers[pSound->dwCurrent];
        
        hr = pDSB->GetStatus( &dwStatus);
        if( FAILED(hr) )
            dwStatus = 0;

        if( dwStatus & DSBSTATUS_PLAYING )
        {
            pDSB->Stop();
            pDSB->SetCurrentPosition( 0 );
        }
    }

    if( dwStatus & DSBSTATUS_BUFFERLOST )
    {
        if( FAILED( pDSB->Restore() ) )
            return NULL;
            
        if( FAILED( DSUtil_FillSoundBuffer( pDSB, pSound->pbWaveData,
                                            pSound->cbWaveSize ) ) )
            return NULL;
    }

    return pDSB;
}




//-----------------------------------------------------------------------------
// Name: DSUtil_PlaySound()
// Desc: 
//-----------------------------------------------------------------------------
HRESULT DSUtil_PlaySound( SoundObject* pSound, DWORD dwPlayFlags )
{
    if( NULL == pSound )
        return E_FAIL;

    if( !(dwPlayFlags & DSBPLAY_LOOPING) || (pSound->dwNumBuffers == 1) )
    {
        LPDIRECTSOUNDBUFFER pDSB = DSUtil_GetFreeSoundBuffer( pSound );
        if( pDSB )
        {
            if( SUCCEEDED( pDSB->Play( 0, 0, dwPlayFlags ) ) )
                return S_OK;
        }
    }

    return E_FAIL;
}




//-----------------------------------------------------------------------------
// Name: DSUtil_StopSound()
// Desc: 
//-----------------------------------------------------------------------------
HRESULT DSUtil_StopSound( SoundObject* pSound )
{
    if( NULL == pSound )
        return E_FAIL;

    for( DWORD i=0; i<pSound->dwNumBuffers; i++ )
    {
        pSound->pdsbBuffers[i]->Stop();
        pSound->pdsbBuffers[i]->SetCurrentPosition( 0 );
    }

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: 
// Desc: 
//-----------------------------------------------------------------------------
HRESULT DSUtil_FillSoundBuffer( LPDIRECTSOUNDBUFFER pDSB, BYTE* pbWaveData,
                                DWORD dwWaveSize )
{
    VOID* pMem1;
    VOID* pMem2;
    DWORD dwSize1;
    DWORD dwSize2;

    if( NULL == pDSB || NULL == pbWaveData || 0 == dwWaveSize )
        return E_FAIL;

    if( FAILED( pDSB->Lock( 0, dwWaveSize, &pMem1, &dwSize1, &pMem2,
                            &dwSize2, 0 ) ) )
        return E_FAIL;

    if( 0 != dwSize1 ) CopyMemory( pMem1, pbWaveData, dwSize1 );
    if( 0 != dwSize2 ) CopyMemory( pMem2, pbWaveData+dwSize1, dwSize2 );

    pDSB->Unlock( pMem1, dwSize1, pMem2, dwSize2);

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: 
// Desc: 
//-----------------------------------------------------------------------------
HRESULT DSUtil_ParseWaveResource( VOID* pvRes, WAVEFORMATEX** ppWaveHeader,
                                  BYTE** ppbWaveData, DWORD* pcbWaveSize )
{
    DWORD* pdw;
    DWORD* pdwEnd;
    DWORD  dwRiff;
    DWORD  dwType;
    DWORD  dwLength;

    if( ppWaveHeader )
        *ppWaveHeader = NULL;

    if( ppbWaveData )
        *ppbWaveData = NULL;

    if( pcbWaveSize )
        *pcbWaveSize = 0;

    pdw      = (DWORD*)pvRes;
    dwRiff   = *pdw++;
    dwLength = *pdw++;
    dwType   = *pdw++;

    if( dwRiff != mmioFOURCC('R', 'I', 'F', 'F') )
        return E_FAIL;

    if( dwType != mmioFOURCC('W', 'A', 'V', 'E') )
        return E_FAIL;

    pdwEnd = (DWORD *)((BYTE *)pdw + dwLength-4);

    while( pdw < pdwEnd )
    {
        dwType   = *pdw++;
        dwLength = *pdw++;

        if( dwType == mmioFOURCC('f', 'm', 't', ' ') )
        {
            if (ppWaveHeader && !*ppWaveHeader)
            {
                if( dwLength < sizeof(WAVEFORMAT) )
                    return E_FAIL;

                *ppWaveHeader = (WAVEFORMATEX*)pdw;

                if( (!ppbWaveData || *ppbWaveData) &&
                    (!pcbWaveSize || *pcbWaveSize) )
                {
                    return S_OK;
                }
            }
        }

        if( dwType == mmioFOURCC('d', 'a', 't', 'a') )
        {
            if( (ppbWaveData && !*ppbWaveData) ||
                (pcbWaveSize && !*pcbWaveSize) )
            {
                if( ppbWaveData )
                    *ppbWaveData = (BYTE*)pdw;

                if( pcbWaveSize )
                    *pcbWaveSize = dwLength;

                if( !ppWaveHeader || *ppWaveHeader )
                    return S_OK;
            }
        }

        pdw = (DWORD*)( (BYTE*)pdw + ((dwLength+1)&~1) );
    }

    return E_FAIL;
}





//-----------------------------------------------------------------------------
// Name: DSUtil_PlayPannedSound()
// Desc: Play a sound, but first set the panning according to where the
//       object is on the screen. fScreenXPos is between -1.0f (left) and
//       1.0f (right).
//-----------------------------------------------------------------------------
VOID DSUtil_PlayPannedSound( SoundObject* pSound, FLOAT fScreenXPos )
{
    LPDIRECTSOUNDBUFFER pDSB = DSUtil_GetFreeSoundBuffer( pSound );

    if( pDSB )
    {
        pDSB->SetPan( (LONG)( 10000L * fScreenXPos ) );
        pDSB->Play( 0, 0, 0 );
    }
}




//-----------------------------------------------------------------------------
// Name: DSUtil_InitDirectSound()
// Desc: 
//-----------------------------------------------------------------------------
HRESULT DSUtil_InitDirectSound( HWND hWnd )
{
    if( FAILED( DirectSoundCreate( NULL, &g_pDS, NULL ) ) )
        return E_FAIL;

    if( FAILED( g_pDS->SetCooperativeLevel( hWnd, DSSCL_NORMAL ) ) )
    {
        g_pDS->Release();
        g_pDS = NULL;
        return E_FAIL;
    }

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: DSUtil_FreeDirectSound()
// Desc: 
//-----------------------------------------------------------------------------
VOID DSUtil_FreeDirectSound()
{
    if( g_pDS )
        g_pDS->Release();
    g_pDS = NULL;
}





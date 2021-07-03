/*==========================================================================
 *
 *  Copyright (C) 1995-1999 Microsoft Corporation. All Rights Reserved.
 *
 *  File:       dsutil3d.c
 *  Content:    Routines for dealing with sounds from resources -- this
 *                version has been modified and extended somewhat from the
 *                version found in the SAMPLES\MISC directory.
 *
 *
 ***************************************************************************/

//#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <windowsx.h>
#include <mmsystem.h>
#include <dsound.h>

typedef struct
{
    BYTE *pbWaveData;               // pointer into wave resource (for restore)
    DWORD cbWaveSize;               // size of wave data (for restore)
    int iAlloc;                     // number of buffers.
    int iCurrent;                   // current buffer
    IDirectSoundBuffer* Buffers[1]; // list of buffers

} SNDOBJ, *HSNDOBJ;

#define _HSNDOBJ_DEFINED
#include "dsutil3d.h"

static const char c_szWAV[] = "WAVE";

///////////////////////////////////////////////////////////////////////////////
//
// DSLoadSoundBuffer - Loads a .WAV into a sound buffer and returns the sound buffer
//
///////////////////////////////////////////////////////////////////////////////

IDirectSoundBuffer *DSLoadSoundBuffer(IDirectSound *pDS, LPCTSTR lpName)
{
    IDirectSoundBuffer *pDSB = NULL;
    DSBUFFERDESC dsBD = {0};
    BYTE *pbWaveData;
    void *pvBase;

    dsBD.dwSize = sizeof(dsBD);
    dsBD.dwFlags = DSBCAPS_STATIC | DSBCAPS_CTRLPAN
                        | DSBCAPS_LOCSOFTWARE;

    if (DSGetWaveResource(NULL, lpName, &dsBD.lpwfxFormat, &pbWaveData, &dsBD.dwBufferBytes))
    {
        if (SUCCEEDED(IDirectSound_CreateSoundBuffer(pDS, &dsBD, &pDSB, NULL)))
        {
            if (!DSFillSoundBuffer(pDSB, pbWaveData, dsBD.dwBufferBytes))
            {
                IDirectSoundBuffer_Release(pDSB);
                pDSB = NULL;
            }
        }
        else
        {
            pDSB = NULL;
        }
    } else if (DSGetWaveFile(NULL, lpName, &dsBD.lpwfxFormat, &pbWaveData,
                        &dsBD.dwBufferBytes, &pvBase))
    {
        if (SUCCEEDED(IDirectSound_CreateSoundBuffer(pDS, &dsBD, &pDSB, NULL)))
        {
            if (!DSFillSoundBuffer(pDSB, pbWaveData, dsBD.dwBufferBytes))
            {
                IDirectSoundBuffer_Release(pDSB);
                pDSB = NULL;
            }
        }
        else
        {
            pDSB = NULL;
        }
	UnmapViewOfFile (pvBase);
    }

    return pDSB;
}


///////////////////////////////////////////////////////////////////////////////
//
// DSLoad3DSoundBuffer - Loads a .WAV into a 3D sound buffer and returns the sound buffer
//
///////////////////////////////////////////////////////////////////////////////

IDirectSoundBuffer *DSLoad3DSoundBuffer(IDirectSound *pDS, LPCTSTR lpName)
{
    IDirectSoundBuffer *pDSB = NULL;
    DSBUFFERDESC dsBD = {0};
    BYTE *pbWaveData;
    void *pvBase;

    dsBD.dwSize = sizeof(dsBD);
    dsBD.dwFlags = DSBCAPS_STATIC | DSBCAPS_CTRL3D | DSBCAPS_CTRLVOLUME
                        | DSBCAPS_CTRLFREQUENCY | DSBCAPS_LOCSOFTWARE
                        | DSBCAPS_STICKYFOCUS;

    if (DSGetWaveResource(NULL, lpName, &dsBD.lpwfxFormat, &pbWaveData, &dsBD.dwBufferBytes))
    {
        if (SUCCEEDED(IDirectSound_CreateSoundBuffer(pDS, &dsBD, &pDSB, NULL)))
        {
            if (!DSFillSoundBuffer(pDSB, pbWaveData, dsBD.dwBufferBytes))
            {
                IDirectSoundBuffer_Release(pDSB);
                pDSB = NULL;
            }
        }
        else
        {
            pDSB = NULL;
        }
    } else if (DSGetWaveFile(NULL, lpName, &dsBD.lpwfxFormat, &pbWaveData,
                        &dsBD.dwBufferBytes, &pvBase))
    {
        if (SUCCEEDED(IDirectSound_CreateSoundBuffer(pDS, &dsBD, &pDSB, NULL)))
        {
            if (!DSFillSoundBuffer(pDSB, pbWaveData, dsBD.dwBufferBytes))
            {
                IDirectSoundBuffer_Release(pDSB);
                pDSB = NULL;
            }
        }
        else
        {
            pDSB = NULL;
        }
	UnmapViewOfFile (pvBase);
    }

    return pDSB;
}


///////////////////////////////////////////////////////////////////////////////
//
// DSReloadSoundBuffer
//
///////////////////////////////////////////////////////////////////////////////

BOOL DSReloadSoundBuffer(IDirectSoundBuffer *pDSB, LPCTSTR lpName)
{
    BOOL result=FALSE;
    BYTE *pbWaveData;
    DWORD cbWaveSize;
    void *pvBase;

    if (DSGetWaveResource(NULL, lpName, NULL, &pbWaveData, &cbWaveSize))
    {
        if (SUCCEEDED(IDirectSoundBuffer_Restore(pDSB)) &&
            DSFillSoundBuffer(pDSB, pbWaveData, cbWaveSize))
	{
	    result = TRUE;
        }
    } else if( DSGetWaveFile(NULL, lpName, NULL, &pbWaveData, &cbWaveSize, &pvBase))
    {
        if (SUCCEEDED(IDirectSoundBuffer_Restore(pDSB)) &&
            DSFillSoundBuffer(pDSB, pbWaveData, cbWaveSize))
	{
	    result = TRUE;
        }
        UnmapViewOfFile (pvBase);
    }

    return result;
}

///////////////////////////////////////////////////////////////////////////////
//
// DSGetWaveResource
//
///////////////////////////////////////////////////////////////////////////////
BOOL DSGetWaveResource(HMODULE hModule, LPCTSTR lpName,
    WAVEFORMATEX **ppWaveHeader, BYTE **ppbWaveData, DWORD *pcbWaveSize)
{
    HRSRC hResInfo;
    HGLOBAL hResData;
    void * pvRes;

    if (((hResInfo = FindResource(hModule, lpName, c_szWAV)) != NULL) &&
        ((hResData = LoadResource(hModule, hResInfo)) != NULL) &&
        ((pvRes = LockResource(hResData)) != NULL) &&
        DSParseWaveResource(pvRes, ppWaveHeader, ppbWaveData, pcbWaveSize))
    {
        return TRUE;
    }

    return FALSE;

}


///////////////////////////////////////////////////////////////////////////////
//
// DSGetWaveFile
//
///////////////////////////////////////////////////////////////////////////////
BOOL DSGetWaveFile(HMODULE hModule, LPCTSTR lpName,
    WAVEFORMATEX **ppWaveHeader, BYTE **ppbWaveData, DWORD *pcbWaveSize,
	void **ppvBase)
{
    void *pvRes;
    HANDLE hFile, hMapping;

    *ppvBase = NULL;

    hFile = CreateFile (lpName, GENERIC_READ, FILE_SHARE_READ, 
	                    NULL, OPEN_EXISTING, 0, NULL);
    if (hFile == INVALID_HANDLE_VALUE) return FALSE;

    hMapping = CreateFileMapping (hFile, NULL, PAGE_READONLY, 0, 0, NULL);
    if (hMapping == INVALID_HANDLE_VALUE)
    {
        CloseHandle (hFile); 
        return FALSE;
    }

    CloseHandle (hFile);

    pvRes = MapViewOfFile (hMapping, FILE_MAP_READ, 0, 0, 0);
    if (pvRes == NULL)
    {
        CloseHandle (hMapping);
        return FALSE;
    }

        CloseHandle (hMapping);

    if (DSParseWaveResource(pvRes, ppWaveHeader, ppbWaveData, pcbWaveSize) == FALSE)
    {
        UnmapViewOfFile (pvRes);
        return FALSE;
    }

    *ppvBase = pvRes;
    return TRUE;
}


///////////////////////////////////////////////////////////////////////////////
// SndObj fns
///////////////////////////////////////////////////////////////////////////////

SNDOBJ *SndObjCreate(IDirectSound *pDS, LPCTSTR lpName, int iConcurrent)
{
    SNDOBJ *pSO = NULL;
    LPWAVEFORMATEX pWaveHeader;
    BOOL fFound = FALSE;
    BYTE *pbData;
    DWORD cbData;
    void * pvBase;

    if (DSGetWaveResource(NULL, lpName, &pWaveHeader, &pbData, &cbData))
        fFound = TRUE;
    if (DSGetWaveFile(NULL, lpName, &pWaveHeader, &pbData, &cbData, &pvBase))
    {
        fFound = TRUE;
        UnmapViewOfFile( pvBase );
    }
    if( fFound )
    {
        if (iConcurrent < 1)
            iConcurrent = 1;

        if ((pSO = (SNDOBJ *)LocalAlloc(LPTR, sizeof(SNDOBJ) +
	    (iConcurrent-1) * sizeof(IDirectSoundBuffer *))) != NULL)
        {
            int i;

            pSO->iAlloc = iConcurrent;
            pSO->pbWaveData = pbData;
            pSO->cbWaveSize = cbData;
            pSO->Buffers[0] = DSLoadSoundBuffer(pDS, lpName);

	    for (i = 1; i < pSO->iAlloc; i++)
	    {
		if (FAILED(IDirectSound_DuplicateSoundBuffer(pDS,
                    pSO->Buffers[0], &pSO->Buffers[i])))
                {
		    pSO->Buffers[i] = DSLoadSoundBuffer(pDS, lpName);
		    if (!pSO->Buffers[i]) {
			SndObjDestroy(pSO);
			pSO = NULL;
			break;
		    }
		}
            }
        }
    }

    return pSO;
}

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

void SndObjDestroy(SNDOBJ *pSO)
{
    if (pSO)
    {
        int i;

        for (i=0; i<pSO->iAlloc; i++)
        {
            if (pSO->Buffers[i])
            {
                IDirectSoundBuffer_Release(pSO->Buffers[i]);
                pSO->Buffers[i] = NULL;
            }
        }
        LocalFree((HANDLE)pSO);
    }
}

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

IDirectSoundBuffer *SndObjGetFreeBuffer(SNDOBJ *pSO)
{
    IDirectSoundBuffer *pDSB;

    if (pSO == NULL)
        return NULL;

    if (pDSB = pSO->Buffers[pSO->iCurrent])
    {
        HRESULT hres;
        DWORD dwStatus;

        hres = IDirectSoundBuffer_GetStatus(pDSB, &dwStatus);

        if (FAILED(hres))
            dwStatus = 0;

        if ((dwStatus & DSBSTATUS_PLAYING) == DSBSTATUS_PLAYING)
        {
            if (pSO->iAlloc > 1)
            {
                if (++pSO->iCurrent >= pSO->iAlloc)
                    pSO->iCurrent = 0;

                pDSB = pSO->Buffers[pSO->iCurrent];
                hres = IDirectSoundBuffer_GetStatus(pDSB, &dwStatus);

                if (SUCCEEDED(hres) && (dwStatus & DSBSTATUS_PLAYING) == DSBSTATUS_PLAYING)
                {
                    IDirectSoundBuffer_Stop(pDSB);
                    IDirectSoundBuffer_SetCurrentPosition(pDSB, 0);
                }
            }
            else
            {
                pDSB = NULL;
            }
        }

        if (pDSB && (dwStatus & DSBSTATUS_BUFFERLOST))
        {
            if (FAILED(IDirectSoundBuffer_Restore(pDSB)) ||
                !DSFillSoundBuffer(pDSB, pSO->pbWaveData, pSO->cbWaveSize))
            {
                pDSB = NULL;
            }
        }
    }

    return pDSB;
}

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

BOOL SndObjPlay(SNDOBJ *pSO, DWORD dwPlayFlags)
{
    BOOL result = FALSE;

    if (pSO == NULL)
        return FALSE;

    if ((!(dwPlayFlags & DSBPLAY_LOOPING) || (pSO->iAlloc == 1)))
    {
	IDirectSoundBuffer *pDSB = SndObjGetFreeBuffer(pSO);
	if (pDSB != NULL) {
	    result = SUCCEEDED(IDirectSoundBuffer_Play(pDSB, 0, 0, dwPlayFlags));
	}
    }

    return result;
}

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

BOOL SndObjStop(SNDOBJ *pSO)
{
    int i;

    if (pSO == NULL)
        return FALSE;

    for (i=0; i<pSO->iAlloc; i++)
    {
        IDirectSoundBuffer_Stop(pSO->Buffers[i]);
        IDirectSoundBuffer_SetCurrentPosition(pSO->Buffers[i], 0);
    }

    return TRUE;
}

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

BOOL DSFillSoundBuffer(IDirectSoundBuffer *pDSB, BYTE *pbWaveData, DWORD cbWaveSize)
{
    if (pDSB && pbWaveData && cbWaveSize)
    {
        LPVOID pMem1, pMem2;
        DWORD dwSize1, dwSize2;

        if (SUCCEEDED(IDirectSoundBuffer_Lock(pDSB, 0, cbWaveSize,
            &pMem1, &dwSize1, &pMem2, &dwSize2, 0)))
        {
            CopyMemory(pMem1, pbWaveData, dwSize1);

            if ( 0 != dwSize2 )
                CopyMemory(pMem2, pbWaveData+dwSize1, dwSize2);

            IDirectSoundBuffer_Unlock(pDSB, pMem1, dwSize1, pMem2, dwSize2);
            return TRUE;
        }
    }

    return FALSE;
}

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

BOOL DSParseWaveResource(void *pvRes, WAVEFORMATEX **ppWaveHeader, BYTE **ppbWaveData,DWORD *pcbWaveSize)
{
    DWORD *pdw;
    DWORD *pdwEnd;
    DWORD dwRiff;
    DWORD dwType;
    DWORD dwLength;

    if (ppWaveHeader)
        *ppWaveHeader = NULL;

    if (ppbWaveData)
        *ppbWaveData = NULL;

    if (pcbWaveSize)
        *pcbWaveSize = 0;

    pdw = (DWORD *)pvRes;
    dwRiff = *pdw++;
    dwLength = *pdw++;
    dwType = *pdw++;

    if (dwRiff != mmioFOURCC('R', 'I', 'F', 'F'))
        goto exit;      // not even RIFF

    if (dwType != mmioFOURCC('W', 'A', 'V', 'E'))
        goto exit;      // not a WAV

    pdwEnd = (DWORD *)((BYTE *)pdw + dwLength-4);

    while (pdw < pdwEnd)
    {
        dwType = *pdw++;
        dwLength = *pdw++;

        switch (dwType)
        {
        case mmioFOURCC('f', 'm', 't', ' '):
            if (ppWaveHeader && !*ppWaveHeader)
            {
                if (dwLength < sizeof(WAVEFORMAT))
                    goto exit;      // not a WAV

                *ppWaveHeader = (WAVEFORMATEX *)pdw;

                if ((!ppbWaveData || *ppbWaveData) &&
                    (!pcbWaveSize || *pcbWaveSize))
                {
                    return TRUE;
                }
            }
            break;

        case mmioFOURCC('d', 'a', 't', 'a'):
            if ((ppbWaveData && !*ppbWaveData) ||
                (pcbWaveSize && !*pcbWaveSize))
            {
                if (ppbWaveData)
                    *ppbWaveData = (LPBYTE)pdw;

                if (pcbWaveSize)
                    *pcbWaveSize = dwLength;

                if (!ppWaveHeader || *ppWaveHeader)
                    return TRUE;
            }
            break;
        }

        pdw = (DWORD *)((BYTE *)pdw + ((dwLength+1)&~1));
    }

exit:
    return FALSE;
}

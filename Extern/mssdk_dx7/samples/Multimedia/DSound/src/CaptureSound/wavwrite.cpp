//-----------------------------------------------------------------------------
// File: WavWrite.cpp
//
// Desc: Wave file support for loading and playing Wave files using DirectSound 
//       buffers.
//
// Copyright (c) 1999 Microsoft Corp. All rights reserved.
//-----------------------------------------------------------------------------
#include <windows.h>
#include "WavWrite.h"




//-----------------------------------------------------------------------------
// Defines, constants, and global variables
//-----------------------------------------------------------------------------
#define SAFE_DELETE(p)  { if(p) { delete (p);     (p)=NULL; } }
#define SAFE_RELEASE(p) { if(p) { (p)->Release(); (p)=NULL; } }




//-----------------------------------------------------------------------------
// Name: WriteMMIO()
// Desc: Support function for reading from a multimedia I/O stream
//-----------------------------------------------------------------------------
HRESULT WriteMMIO( HMMIO hmmioOut, MMCKINFO* pckOut, MMCKINFO* pckOutRIFF, 
                   WAVEFORMATEX* pwfxDest )
{
    DWORD    dwFactChunk; // Contains the actual fact chunk. Garbage until WaveCloseWriteFile.
    MMCKINFO ckOut1;
    
    dwFactChunk = (DWORD)-1;

    // Create the output file RIFF chunk of form type 'WAVE'.
    pckOutRIFF->fccType = mmioFOURCC('W', 'A', 'V', 'E');       
    pckOutRIFF->cksize = 0;

    if( 0 != mmioCreateChunk( hmmioOut, pckOutRIFF, MMIO_CREATERIFF ) )
        return E_FAIL;
    
    // We are now descended into the 'RIFF' chunk we just created.
    // Now create the 'fmt ' chunk. Since we know the size of this chunk,
    // specify it in the MMCKINFO structure so MMIO doesn't have to seek
    // back and set the chunk size after ascending from the chunk.
    pckOut->ckid = mmioFOURCC('f', 'm', 't', ' ');
    pckOut->cksize = sizeof(PCMWAVEFORMAT);   

    if( 0 != mmioCreateChunk( hmmioOut, pckOut, 0 ) )
        return E_FAIL;
    
    // Write the PCMWAVEFORMAT structure to the 'fmt ' chunk if its that type. 
    if( pwfxDest->wFormatTag == WAVE_FORMAT_PCM)
    {
        if( mmioWrite( hmmioOut, (HPSTR) pwfxDest, 
                        sizeof(PCMWAVEFORMAT)) != sizeof(PCMWAVEFORMAT))
            return E_FAIL;
    }   
    else 
    {
        // Write the variable length size.
        if( (UINT)mmioWrite( hmmioOut, (HPSTR) pwfxDest, 
                             sizeof(*pwfxDest) + pwfxDest->cbSize ) != 
                             ( sizeof(*pwfxDest) + pwfxDest->cbSize ) )
            return E_FAIL;
    }  
    
    // Ascend out of the 'fmt ' chunk, back into the 'RIFF' chunk.
    if( 0 != mmioAscend( hmmioOut, pckOut, 0 ) )
        return E_FAIL;
    
    // Now create the fact chunk, not required for PCM but nice to have.  This is filled
    // in when the close routine is called.
    ckOut1.ckid = mmioFOURCC('f', 'a', 'c', 't');
    ckOut1.cksize = 0;

    if( 0 != mmioCreateChunk( hmmioOut, &ckOut1, 0 ) )
        return E_FAIL;
    
    if( mmioWrite( hmmioOut, (HPSTR)&dwFactChunk, sizeof(dwFactChunk)) != 
                    sizeof(dwFactChunk) )
         return E_FAIL;
    
    // Now ascend out of the fact chunk...
    if( 0 != mmioAscend( hmmioOut, &ckOut1, 0 ) )
        return E_FAIL;
       
    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: WaveOpenFile()
// Desc: This routine will create a wave file for writing.  This will 
//       automatically overwrite any existing file with the same name
//-----------------------------------------------------------------------------
HRESULT WaveOpenFile( CHAR* strFileName, HMMIO* phmmioOut, 
                      WAVEFORMATEX* pwfxDest,
                      MMCKINFO *pckOut, MMCKINFO *pckOutRIFF )
{
    HRESULT hr;
    HMMIO   hmmioOut = NULL;

    if( NULL == ( hmmioOut = mmioOpen( strFileName, NULL, MMIO_ALLOCBUF  | 
                                                          MMIO_READWRITE | 
                                                          MMIO_CREATE ) ) )
        return E_FAIL;

    if( FAILED( hr = WriteMMIO( hmmioOut, pckOut, pckOutRIFF, pwfxDest ) ) )
    {
        mmioClose( hmmioOut, 0 );
        return hr;
    }

    *phmmioOut = hmmioOut;

    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: WaveStartDataRead()
// Desc: This routine has to be called before any data is written to the wave output file, via wavewritefile.  This
//       sets up the data to write, and creates the data chunk.
//-----------------------------------------------------------------------------
HRESULT WaveStartDataWrite( HMMIO* phmmioOut, MMCKINFO* pckOut,
                            MMIOINFO* pmmioinfoOut )
{
    // Create the 'data' chunk that holds the waveform samples.  
    pckOut->ckid = mmioFOURCC('d', 'a', 't', 'a');
    pckOut->cksize = 0;

    if( 0 != mmioCreateChunk( *phmmioOut, pckOut, 0 ) ) 
        return E_FAIL;
    
    if( 0 != mmioGetInfo( *phmmioOut, pmmioinfoOut, 0 ) )
        return E_FAIL;
    
    return S_OK;
}




//-----------------------------------------------------------------------------
// Name: WaveWriteFile()
// Desc: This routine will write out data to a wave file. Make sure we're 
//       created the chuck before calling this function.
//-----------------------------------------------------------------------------
HRESULT WaveWriteFile( HMMIO hmmioOut, UINT cbWrite, BYTE* pbSrc,
                       MMCKINFO* pckOut, UINT* cbActualWrite, 
                       MMIOINFO* pmmioinfoOut )
{
    UINT cT;

    *cbActualWrite = 0;
    
    for( cT = 0; cT < cbWrite; cT++ )
    {       
        if( pmmioinfoOut->pchNext == pmmioinfoOut->pchEndWrite )
        {
            pmmioinfoOut->dwFlags |= MMIO_DIRTY;
            if( 0 != mmioAdvance( hmmioOut, pmmioinfoOut, MMIO_WRITE ) )
                return E_FAIL;
        }

        *((BYTE*)pmmioinfoOut->pchNext) = *((BYTE*)pbSrc+cT);
        (BYTE*)pmmioinfoOut->pchNext++;

        (*cbActualWrite)++;
    }

    return S_OK;
}



  
//-----------------------------------------------------------------------------
// Name: WaveCloseWriteFile()
// Desc: This routine will close a wave file used for writing.  
//-----------------------------------------------------------------------------
HRESULT WaveCloseWriteFile( HMMIO hmmioOut,       
                            MMCKINFO *pckOut,       
                            MMCKINFO *pckOutRIFF,   
                            MMIOINFO *pmmioinfoOut, 
                            DWORD dwSamples )
{    
    pmmioinfoOut->dwFlags |= MMIO_DIRTY;

    if( 0 != mmioSetInfo( hmmioOut, pmmioinfoOut, 0 ) )
        return E_FAIL;
    
    // Ascend the output file out of the 'data' chunk -- this will cause
    // the chunk size of the 'data' chunk to be written.
    if( 0 != mmioAscend( hmmioOut, pckOut, 0 ) )
        return E_FAIL;
    
    // Do this here instead...
    if( 0 != mmioAscend( hmmioOut, pckOutRIFF, 0 ) )
        return E_FAIL;
        
    mmioSeek( hmmioOut, 0, SEEK_SET );

    if( 0 != (INT)mmioDescend( hmmioOut, pckOutRIFF, NULL, 0 ) )
        return E_FAIL;
    
    pckOut->ckid = mmioFOURCC('f', 'a', 'c', 't');

    if( 0 == mmioDescend( hmmioOut, pckOut, pckOutRIFF, MMIO_FINDCHUNK ) ) 
    {
        mmioWrite( hmmioOut, (HPSTR)&dwSamples, sizeof(DWORD) );
        mmioAscend( hmmioOut, pckOut, 0 ); 
    }
    
    // Ascend the output file out of the 'RIFF' chunk -- this will cause
    // the chunk size of the 'RIFF' chunk to be written.
    if( 0 != mmioAscend( hmmioOut, pckOutRIFF, 0 ) )
        return E_FAIL;
    
    mmioClose( hmmioOut, 0 );

    return S_OK;   
}




//-----------------------------------------------------------------------------
// Name: CWaveSoundWrite()
// Desc: Constructs the class
//-----------------------------------------------------------------------------
CWaveSoundWrite::CWaveSoundWrite()
{
    m_hmmioOut = NULL;
}




//-----------------------------------------------------------------------------
// Name: ~CWaveSoundWrite()
// Desc: Destructs the class
//-----------------------------------------------------------------------------
CWaveSoundWrite::~CWaveSoundWrite()
{
    Close( 0 );
}




//-----------------------------------------------------------------------------
// Name: Open()
// Desc: Opens a wave file for reading
//-----------------------------------------------------------------------------
HRESULT CWaveSoundWrite::Open( CHAR* strFilename, WAVEFORMATEX *pwfxDest )
{
    HRESULT  hr;

    if( FAILED( hr = WaveOpenFile( strFilename, &m_hmmioOut, 
                                   pwfxDest, &m_ckOut, &m_ckOutRIFF ) ) )
        return hr;
                        
    if( FAILED( hr = Reset() ) )
        return hr;

    return hr;
}




//-----------------------------------------------------------------------------
// Name: Reset()
// Desc: Resets the internal m_ckOut pointer so writing starts from the 
//       beginning of the file again 
//-----------------------------------------------------------------------------
HRESULT CWaveSoundWrite::Reset()
{
    return WaveStartDataWrite( &m_hmmioOut, &m_ckOut,
                               &m_mmioinfoOut );
}




//-----------------------------------------------------------------------------
// Name: Write()
// Desc: Writes data to the open wave file
//-----------------------------------------------------------------------------
HRESULT CWaveSoundWrite::Write( UINT nSizeToWrite, BYTE* pbData, UINT* pnSizeWrote )
{
    return WaveWriteFile( m_hmmioOut, nSizeToWrite, pbData, &m_ckOut, 
                          pnSizeWrote, &m_mmioinfoOut );
}




//-----------------------------------------------------------------------------
// Name: Close()
// Desc: Closes an open wave file 
//-----------------------------------------------------------------------------
HRESULT CWaveSoundWrite::Close( DWORD dwSamples )
{
    return WaveCloseWriteFile( m_hmmioOut, &m_ckOut, &m_ckOutRIFF, 
                               &m_mmioinfoOut, dwSamples );
}



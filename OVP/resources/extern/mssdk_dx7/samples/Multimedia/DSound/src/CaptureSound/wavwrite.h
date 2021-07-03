//-----------------------------------------------------------------------------
// File: WavWrite.h
//
// Desc: Support for loading and playing Wave files using DirectSound sound
//       buffers.
//
// Copyright (c) 1999 Microsoft Corp. All rights reserved.
//-----------------------------------------------------------------------------
#ifndef WAVE_WRITE_H
#define WAVE_WRITE_H


#include <mmreg.h>
#include <mmsystem.h>


//-----------------------------------------------------------------------------
// Name: class CWaveSoundWrite
// Desc: A class to write in sound data to a Wave file
//-----------------------------------------------------------------------------
class CWaveSoundWrite
{
public:
    HMMIO    m_hmmioOut;        // MM I/O handle for the WAVE
    MMCKINFO m_ckOut;           // Multimedia RIFF chunk
    MMCKINFO m_ckOutRIFF;       // Use in opening a WAVE file
    MMIOINFO m_mmioinfoOut;

public:
    CWaveSoundWrite();
    ~CWaveSoundWrite();

    HRESULT Open( CHAR* strFilename, WAVEFORMATEX *pwfxDest );
    HRESULT Reset();
    HRESULT Write( UINT nSizeToWrite, BYTE* pbData, UINT* pnSizeWrote );
    HRESULT Close( DWORD dwSamples );

};


#endif WAVE_WRITE_H




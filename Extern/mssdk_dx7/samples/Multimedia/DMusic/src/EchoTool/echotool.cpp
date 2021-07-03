//-----------------------------------------------------------------------------
// File: EchoTool.cpp
//
// Desc: Implements an object based on IDirectMusicTool
// that provides echoing effects.
//
// Copyright (c) 1998-1999 Microsoft Corporation. All rights reserved.
//-----------------------------------------------------------------------------


#include <dmusici.h>
#include "EchoTool.h"

CEchoTool::CEchoTool()
{
    m_cRef = 1; // set to 1 so one call to Release() will free this
    m_dwEchoNum = 3; // default to 3 echoes per note
    m_mtDelay = DMUS_PPQ / 2; // default to 8th note echoes
    InitializeCriticalSection(&m_CrSec);
}

CEchoTool::~CEchoTool()
{
    DeleteCriticalSection(&m_CrSec);
}

/////////////////////////////////////////////////////////////////
// IUnknown

STDMETHODIMP CEchoTool::QueryInterface(const IID &iid, void **ppv)
{
    if (iid == IID_IUnknown || iid == IID_IDirectMusicTool)
    {
        *ppv = static_cast<IDirectMusicTool*>(this);
    } 
    else
    {
        *ppv = NULL;
        return E_NOINTERFACE;
    }
    
    reinterpret_cast<IUnknown*>(this)->AddRef();
    return S_OK;
}

STDMETHODIMP_(ULONG) CEchoTool::AddRef()
{
    return InterlockedIncrement(&m_cRef);
}

STDMETHODIMP_(ULONG) CEchoTool::Release()
{
    if( 0 == InterlockedDecrement(&m_cRef) )
    {
        delete this;
        return 0;
    }

    return m_cRef;
}

/////////////////////////////////////////////////////////////////
// IDirectMusicTool

HRESULT STDMETHODCALLTYPE CEchoTool::Init( IDirectMusicGraph* pGraph )
{
    // This tool has no need to do any type of initialization.
    return E_NOTIMPL;
}

HRESULT STDMETHODCALLTYPE CEchoTool::GetMsgDeliveryType( DWORD* pdwDeliveryType )
{
    // This tool wants messages immediately.
    // This is the default, so returning E_NOTIMPL
    // would work. The other method is to specifically
    // set *pdwDeliveryType to the delivery type, DMUS_PMSGF_TOOL_IMMEDIATE,
    // DMUS_PMSGF_TOOL_QUEUE, or DMUS_PMSGF_TOOL_ATTIME.
    
    *pdwDeliveryType = DMUS_PMSGF_TOOL_IMMEDIATE;
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CEchoTool::GetMediaTypeArraySize( DWORD* pdwNumElements )
{
    // This tool only wants note messages, patch messages, and MIDI messages, so set
    // *pdwNumElements to 3.
    
    *pdwNumElements = 3;
    return S_OK;
}

HRESULT STDMETHODCALLTYPE CEchoTool::GetMediaTypes( DWORD** padwMediaTypes, 
                                                    DWORD dwNumElements )
{
    // Fill in the array padwMediaTypes with the type of
    // messages this tool wants to process. In this case,
    // dwNumElements will be 3, since that is what this
    // tool returns from GetMediaTypeArraySize().
    
    if( dwNumElements == 3 )
    {
        // set the elements in the array to DMUS_PMSGT_NOTE,
        // DMUS_PMSGT_MIDI, and DMUS_PMSGT_PATCH
        (*padwMediaTypes)[0] = DMUS_PMSGT_NOTE;
        (*padwMediaTypes)[1] = DMUS_PMSGT_MIDI;
        (*padwMediaTypes)[2] = DMUS_PMSGT_PATCH;
        return S_OK;
    }
    else
    {
        // this should never happen
        return E_FAIL;
    }
}

HRESULT STDMETHODCALLTYPE CEchoTool::ProcessPMsg( IDirectMusicPerformance* pPerf, 
                                                  DMUS_PMSG* pPMsg )
{
    DMUS_NOTE_PMSG* pNote;
    DWORD dwCount;
    DWORD dwEchoNum;
    MUSIC_TIME mtDelay;
    
    // SetEchoNum() and SetDelay() use these member variables,
    // so use a critical section to make them thread-safe.
    EnterCriticalSection(&m_CrSec);
    dwEchoNum = m_dwEchoNum;
    mtDelay = m_mtDelay;
    LeaveCriticalSection(&m_CrSec);
    
    // returning S_FREE frees the message. If StampPMsg()
    // fails, there is no destination for this message so
    // free it.
    if(( NULL == pPMsg->pGraph ) ||
        FAILED(pPMsg->pGraph->StampPMsg(pPMsg)))
    {
        return DMUS_S_FREE;
    }
    
    // The Tool is set up to only receive messages of types
    // DMUS_PMSGT_NOTE, DMUS_PMSGT_MIDI, or DMUS_PMSGT_PATCH
    if( pPMsg->dwType == DMUS_PMSGT_MIDI )
    {
        // copy midi messages into the echo channels
        for( dwCount = 1; dwCount <= dwEchoNum; dwCount++ )
        {
            DMUS_MIDI_PMSG* pMidi;
            if( SUCCEEDED( pPerf->AllocPMsg( sizeof(DMUS_MIDI_PMSG),
                (DMUS_PMSG**)&pMidi )))
            {
                // copy the original message into this message
                memcpy( pMidi, pPMsg, sizeof(DMUS_MIDI_PMSG) );
                // addref or clear out any fields that contain
                // or may contain pointers to objects
                if( pMidi->pTool ) pMidi->pTool->AddRef();
                if( pMidi->pGraph ) pMidi->pGraph->AddRef();
                pMidi->punkUser = NULL;
                // set the PChannel so the message goes to the
                // next higher group
                pMidi->dwPChannel = pMidi->dwPChannel + 
                    (16*dwCount);
                // add to the time of the echoed message
                pMidi->mtTime += (dwCount * mtDelay);
                // set the message so only MUSIC_TIME is valid.
                // REFERENCE_TIME will be recomputed inside
                // SendPMsg()
                pMidi->dwFlags = DMUS_PMSGF_MUSICTIME;
                // queue the message
                pPerf->SendPMsg( (DMUS_PMSG*)pMidi );
            }
        }
    }
    else if( pPMsg->dwType == DMUS_PMSGT_PATCH )
    {
        // Just copy the patch message and send it to the echo channels.
        // We do this even when the current echo count, dwEchoNum, is
        // lower because we always want the PChannels set up with the
        // correct instruments.
        for( dwCount = 1; dwCount <= MAX_ECHOES; dwCount++ )
        {
            DMUS_PATCH_PMSG* pPatch;
            if( SUCCEEDED( pPerf->AllocPMsg( sizeof(DMUS_PATCH_PMSG),
                (DMUS_PMSG**)&pPatch )))
            {
                // copy the original message into this message
                memcpy( pPatch, pPMsg, sizeof(DMUS_PATCH_PMSG) );
                // addref or clear out any fields that contain
                // or may contain pointers to objects
                if( pPatch->pTool ) pPatch->pTool->AddRef();
                if( pPatch->pGraph ) pPatch->pGraph->AddRef();
                pPatch->punkUser = NULL;
                // set the PChannel so the message goes to the
                // next higher group
                pPatch->dwPChannel = pPatch->dwPChannel + 
                    (16*dwCount);
                // add to the time of the echoed message
                pPatch->mtTime += (dwCount * mtDelay);
                // set the message so only MUSIC_TIME is valid.
                // REFERENCE_TIME will be recomputed inside
                // SendPMsg()
                pPatch->dwFlags = DMUS_PMSGF_MUSICTIME;
                // queue the patch
                pPerf->SendPMsg( (DMUS_PMSG*)pPatch );
            }
        }
    }
    else if( pPMsg->dwType == DMUS_PMSGT_NOTE )
    {
        // create a variable to track the next note's velocity
        BYTE bVelocity;
        pNote = (DMUS_NOTE_PMSG*)pPMsg;
        bVelocity = pNote->bVelocity;
        for( dwCount = 1; dwCount <= dwEchoNum; dwCount++ )
        {
            if( SUCCEEDED( pPerf->AllocPMsg( sizeof(DMUS_NOTE_PMSG),
                (DMUS_PMSG**)&pNote )))
            {
                // copy the original note into this message
                memcpy( pNote, pPMsg, sizeof(DMUS_NOTE_PMSG) );
                // addref or clear out any fields that contain
                // or may contain pointers to objects
                if( pNote->pTool ) pNote->pTool->AddRef();
                if( pNote->pGraph ) pNote->pGraph->AddRef();
                pNote->punkUser = NULL;
                // add to the time of the echoed note
                pNote->mtTime += (dwCount * mtDelay);
            
                // reduce the volume of the echoed note
                // percentage of reduction in velocity increases with each echo
                bVelocity = (BYTE) (bVelocity - ((bVelocity * (dwCount * 15))/100));
                
                pNote->bVelocity = bVelocity;
                // set the note so only MUSIC_TIME is valid.
                // REFERENCE_TIME will be recomputed inside
                // SendPMsg()
                pNote->dwFlags = DMUS_PMSGF_MUSICTIME;
                pNote->dwPChannel = pNote->dwPChannel + 
                    (16*dwCount);
                // queue the note
                pPerf->SendPMsg( (DMUS_PMSG*)pNote );
            }
        }
    }
    // return DMUS_S_REQUEUE so the original message is requeued
    return DMUS_S_REQUEUE;
}

HRESULT STDMETHODCALLTYPE CEchoTool::Flush( IDirectMusicPerformance* pPerf, 
                                            DMUS_PMSG* pDMUS_PMSG,
                                            REFERENCE_TIME rt)
{
    // this tool does not need to flush.
    return E_NOTIMPL;
}

/////////////////////////////////////////////////////////////////
// Public class methods

// Set the number of echoes for the Tool
void CEchoTool::SetEchoNum(DWORD dwEchoNum)
{
    // ProcessPMsg() uses m_dwEchoNum, so use a critical
    // section to make it thread-safe.
    if( dwEchoNum <= MAX_ECHOES )
    {
        EnterCriticalSection(&m_CrSec);
        m_dwEchoNum = dwEchoNum;
        LeaveCriticalSection(&m_CrSec);
    }
}

// Set the delay
void CEchoTool::SetDelay(MUSIC_TIME mtDelay)
{
    // ProcessPMsg() uses m_mtDelay, so use a critical
    // section to make it thread-safe.
    EnterCriticalSection(&m_CrSec);
    m_mtDelay = mtDelay;
    LeaveCriticalSection(&m_CrSec);
}

//
// CFrameTimer
// Copyright (c) 1999 Microsoft Corporation. All rights reserved.
//

#include "pch.hpp"


//////////////////////////////////////////////////////////////////////////////
// CFrameTimer implementation //////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////

CFrameTimer::CFrameTimer()
{
    if(!QueryPerformanceFrequency((LARGE_INTEGER *) &m_qwTicksPerSec))
        m_qwTicksPerSec = 1000;

    m_fTicksPerSec  = (float) (__int64) m_qwTicksPerSec;
}


void 
CFrameTimer::Start(float fFramesPerSec)
{
    m_fFramesPerSec = fFramesPerSec;
    m_fSecsPerFrame = 1.0f / fFramesPerSec;
    m_qwTicksPerFrame = m_qwTicksPerSec / (__int64) fFramesPerSec;

    m_qwTicks = GetTicks();
}


void 
CFrameTimer::Frame()
{
    UINT64 qw;
    qw = GetTicks();


    if(qw != m_qwTicks)
    {
        m_qwTicksPerFrame = qw - m_qwTicks;
        m_qwTicks = qw;

        m_fFramesPerSec = 0.75f * m_fFramesPerSec +
            0.25f * (m_fTicksPerSec / (float) (__int64) m_qwTicksPerFrame);

        if(m_fFramesPerSec < 1.0f)
            m_fFramesPerSec = 1.0f;

        m_fSecsPerFrame = 1.0f / m_fFramesPerSec;
    }
}


UINT64
CFrameTimer::GetTicks()
{
    UINT64 qw;

    if(QueryPerformanceCounter((LARGE_INTEGER *) &qw))
    {
        while(!qw)
          QueryPerformanceCounter((LARGE_INTEGER *) &qw);
    }
    else
    {
        qw = (UINT64) timeGetTime();
    }

    return qw;
}
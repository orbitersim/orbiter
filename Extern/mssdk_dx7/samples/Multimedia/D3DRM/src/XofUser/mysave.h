//-----------------------------------------------------------------------------
// File: myguids.h
//
// Desc: Class used to help save user data
//
// Copyright (C) 1998-1999 Microsoft Corporation. All Rights Reserved.
//-----------------------------------------------------------------------------
#ifndef _MYSAVE_H_
#define _MYSAVE_H_

class XOFSave
{
    IDirectXFile *m_pxofapi;
    IDirectXFileSaveObject *m_pxofSave;

    HRESULT save_templates();

    HRESULT save_simple_data();
    HRESULT save_array_data();
    HRESULT save_optional_data();
    HRESULT save_reference_data();

public:
    XOFSave();
    ~XOFSave();

    HRESULT Run(DXFILEFORMAT xFormat);
};

#endif // _MYSAVE_H_

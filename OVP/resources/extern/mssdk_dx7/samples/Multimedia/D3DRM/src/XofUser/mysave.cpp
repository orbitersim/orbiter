//-----------------------------------------------------------------------------
// File: mysave.cpp
//
// Desc: Helper class user to save user data to .X File
//
// Copyright (C) 1998-1999 Microsoft Corporation. All Rights Reserved.
//-----------------------------------------------------------------------------
#define INITGUID
#include <windows.h>
#include <iostream.h>
#include <objbase.h>
#include "dxfile.h"
#include "myguids.h"
#include "mytypes.h"
#include "mysave.h"

XOFSave::XOFSave()
  : m_pxofapi(NULL),
    m_pxofSave(NULL)
{
}

XOFSave::~XOFSave()
{
    if (m_pxofSave)
        m_pxofSave->Release();

    if (m_pxofapi)
        m_pxofapi->Release();
}


HRESULT XOFSave::Run(DXFILEFORMAT xFormat)
{
    HRESULT hr;

    do {
        hr = DirectXFileCreate(&m_pxofapi);

        if (FAILED(hr))
            break;

        hr = m_pxofapi->CreateSaveObject("test.x",
                                         xFormat,
                                         &m_pxofSave);

        if (FAILED(hr))
            break;

        hr = save_templates();

        if (FAILED(hr))
            break;

        hr = save_simple_data();

        if (FAILED(hr))
            break;

        hr = save_array_data();

        if (FAILED(hr))
            break;

        hr = save_optional_data();

        if (FAILED(hr))
            break;

        hr = save_reference_data();

        if (FAILED(hr))
            break;

    } while (FALSE);

    return hr;
}

HRESULT XOFSave::save_templates()
{
    HRESULT hr;
    char *szTemplates = "xof 0303txt 0032\
        template SimpleData { \
            <2b934580-9e9a-11cf-ab39-0020af71e433> \
            DWORD item1;DWORD item2;DWORD item3;} \
        template ArrayData { \
            <2b934581-9e9a-11cf-ab39-0020af71e433> \
            DWORD cItems; array DWORD aItem[2][cItems]; [...] } \
        template RestrictedData { \
            <2b934582-9e9a-11cf-ab39-0020af71e433> \
            DWORD item; [SimpleData]}";

    hr = m_pxofapi->RegisterTemplates(szTemplates, strlen(szTemplates));

    if (SUCCEEDED(hr)) {
        const GUID *aIds[] = {
            &DXFILEOBJ_SimpleData,
            &DXFILEOBJ_ArrayData,
            &DXFILEOBJ_RestrictedData};

        hr = m_pxofSave->SaveTemplates(3, aIds);
    }

    if (FAILED(hr))
        cout << "Save templates test failed.\n";
    else
        cout << "Save templates test passed.\n";

    return hr;
}

HRESULT XOFSave::save_simple_data()
{
    IDirectXFileData *pObj = NULL;
    HRESULT hr;

    do {
        TSimple data;
        data.item1 = 1;
        data.item2 = 2;
        data.item3 = 3;

        hr = m_pxofSave->CreateDataObject(DXFILEOBJ_SimpleData,
                                      "simpledata1",
                                      &DXFILEOBJ_simpledata1,
                                      sizeof(TSimple),
                                      &data,
                                      &pObj);

        if (FAILED(hr))
            break;

        hr = m_pxofSave->SaveData(pObj);

    } while (FALSE);

    if (pObj)
        pObj->Release();

    if (FAILED(hr))
        cout << "Simple data test failed.\n";
    else
        cout << "Simple data test passed.\n";

    return hr;
}

HRESULT XOFSave::save_array_data()
{
    IDirectXFileData *pObj = NULL;
    HRESULT hr;

    do {
        TArray *pData;
        DWORD *p, cbSize;

        cbSize = 7 * sizeof(DWORD);

        pData = (TArray *)malloc(cbSize);

        if (!pData)
        {
            hr = ERROR_OUTOFMEMORY;
            break;
        }

        pData->cItems = 3;

        p = (DWORD *)pData->aItems;

        for (DWORD i = 0; i < pData->cItems; i++)
        {
            for (DWORD j = 0; j < 2; j++)
            {
                *p = i + j;
                p++;
            }
        }

        hr = m_pxofSave->CreateDataObject(DXFILEOBJ_ArrayData,
                                      NULL,
                                      NULL,
                                      cbSize,
                                      pData,
                                      &pObj);

        free(pData);

        if (FAILED(hr))
            break;

        hr = m_pxofSave->SaveData(pObj);

    } while (FALSE);

    if (pObj)
        pObj->Release();

    if (FAILED(hr))
        cout << "Array data test failed.\n";
    else
        cout << "Array data test passed.\n";

    return hr;
}

HRESULT XOFSave::save_optional_data()
{
    IDirectXFileData *pObj = NULL;
    HRESULT hr;

    do {
        TRestricted data1;
        data1.item = 1;

        hr = m_pxofSave->CreateDataObject(DXFILEOBJ_RestrictedData,
                                      NULL,
                                      NULL,
                                      sizeof(TRestricted),
                                      &data1,
                                      &pObj);

        if (FAILED(hr))
            break;

        IDirectXFileData *pObjChild = NULL;

        TSimple data;
        data.item1 = 1;
        data.item2 = 2;
        data.item3 = 3;

        hr = m_pxofSave->CreateDataObject(DXFILEOBJ_SimpleData,
                                      NULL,
                                      NULL,
                                      sizeof(TSimple),
                                      &data,
                                      &pObjChild);

        if (FAILED(hr))
            break;

        hr = pObj->AddDataObject(pObjChild);

        pObjChild->Release();

        if (FAILED(hr))
            break;

        hr = m_pxofSave->SaveData(pObj);

    } while (FALSE);

    if (pObj)
        pObj->Release();

    if (FAILED(hr))
        cout << "Restricted data test failed.\n";
    else
        cout << "Restricted data test passed.\n";

    return hr;
}

HRESULT XOFSave::save_reference_data()
{
    IDirectXFileData *pObj = NULL;
    HRESULT hr;

    do {
        TRestricted data;
        data.item = 1;

        hr = m_pxofSave->CreateDataObject(DXFILEOBJ_RestrictedData,
                                    NULL,
                                    NULL,
                                    sizeof(TRestricted),
                                    &data,
                                    &pObj);

        if (FAILED(hr))
            break;

        IDirectXFileDataReference *pObjChild = NULL;

        hr = pObj->AddDataReference("simpledata1", NULL);

        if (FAILED(hr))
            break;

        hr = m_pxofSave->SaveData(pObj);

    } while (FALSE);

    if (pObj)
        pObj->Release();

    if (FAILED(hr))
        cout << "Reference data test failed.\n";
    else
        cout << "Reference data test passed.\n";

    return hr;
}

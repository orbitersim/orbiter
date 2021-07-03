//-----------------------------------------------------------------------------
// File: xofload.cpp
//
// Desc: Converts a .X File from one format to another
//
// Copyright (C) 1998-1999 Microsoft Corporation. All Rights Reserved.
//-----------------------------------------------------------------------------
#define INITGUID

#include <windows.h>
#include <iostream.h>
#include <objbase.h>
#include "dxfile.h"
#include "rmxftmpl.h"

typedef struct _Header {
    WORD major;
    WORD minor;
    DWORD flags;
} Header;

int main(int argc, char *argv[])
{
    LPDIRECTXFILE lpApi = NULL;
    LPDIRECTXFILEENUMOBJECT lpEnumObj = NULL;
    LPDIRECTXFILESAVEOBJECT lpSaveObj = NULL;
    LPCSTR szSrc;
    DXFILEFORMAT xFormat;
    HRESULT hr;

    // Get filename from command line argument.
    if (argc < 3 || strlen(argv[1]) != 2) {
Usage:  cout << "\nUsage:  convxfmt -t/b/c filename\n\n";
        cout << "Use -t, -b or -c to specify the final format.\n";
        cout << "-t for text, -b for binary and -c for compressed.\n\n";
        cout << "The output file is tmp.x in the working directory.\n";
        return -1;
    }
    if (argv[1][1] == 't' || argv[1][1] == 'T')
        xFormat = DXFILEFORMAT_TEXT;
    else if (argv[1][1] == 'b' || argv[1][1] == 'B')
        xFormat = DXFILEFORMAT_BINARY;
    else if (argv[1][1] == 'c' || argv[1][1] == 'C')
        xFormat = DXFILEFORMAT_COMPRESSED;
    else
        goto Usage;

    szSrc = argv[2];

    // Use do/while(FALSE) to simplify error handlig
    do {

        // Create xofapi object.
        hr = DirectXFileCreate(&lpApi);
        if (FAILED(hr))
            break;

        // Registe templates for d3drm.
        hr = lpApi->RegisterTemplates((LPVOID)D3DRM_XTEMPLATES,
                                      D3DRM_XTEMPLATE_BYTES);
        if (FAILED(hr))
            break;

        // Create enum object.
        hr = lpApi->CreateEnumObject((LPVOID)szSrc,
                                     DXFILELOAD_FROMFILE,
                                     &lpEnumObj);
        if (FAILED(hr))
            break;

        // Create save object.
        hr = lpApi->CreateSaveObject("tmp.x",
                                     xFormat,
                                     &lpSaveObj);
        if (FAILED(hr))
            break;

        // Enumerate top level objects.

        LPDIRECTXFILEDATA lpDataObj;

        while (SUCCEEDED(lpEnumObj->GetNextDataObject(&lpDataObj))) {
            const GUID *type;

            hr = lpDataObj->GetType(&type);
            if (FAILED(hr))
                break;

            // If this is the header we may have to change the type
            if (*type == TID_DXFILEHeader) {

                // Flip the binary/text flag bit.

                Header *pHeader;
                DWORD cbSize;

                hr = lpDataObj->GetData(NULL, &cbSize, (LPVOID *)&pHeader);
                if (FAILED(hr))
                    break;

                if (pHeader->flags & 1) {
                    if (xFormat == DXFILEFORMAT_BINARY)
                        pHeader->flags &= 0xffffffe;
                } else {
                    if (xFormat == DXFILEFORMAT_TEXT)
                        pHeader->flags |= 1;
                }
            }

            // Save the data
            hr = lpSaveObj->SaveData(lpDataObj);
            if (FAILED(hr))
                break;

            // Release the data object
            lpDataObj->Release();
        }

    } while (FALSE);

    // Release outstanding interfaces
    if (lpSaveObj)
        lpSaveObj->Release();

    if (lpEnumObj)
        lpEnumObj->Release();

    if (lpApi)
        lpApi->Release();

    cout << "Done.\n";

    return 0;
}

//-----------------------------------------------------------------------------
// File: xofload.cpp
//
// Desc: Load a .X File using the DirectXFile API
//
// Copyright (C) 1998-1999 Microsoft Corporation. All Rights Reserved.
//-----------------------------------------------------------------------------
#define INITGUID

#include <windows.h>
#include <iostream.h>
#include <objbase.h>
#include "dxfile.h"
#include "rmxfguid.h"
#include "rmxftmpl.h"

int main(int argc, char *argv[])
{
    LPCSTR szFilename;
    LPDIRECTXFILE pxofapi = NULL;
    LPDIRECTXFILEENUMOBJECT pxofenum = NULL;
    HRESULT hr;


    // Get filename from command line argument.
    if (argc <2 || argc > 2) {
        cout << "XofLoad - Sample program to load a .X File\n";
        cout << "Usage : xofload <filename>\n";
	return (0);
    }
    szFilename = argv[1];

    // Using a do/while(FALSE) loop instead of goto for errors...
    do {
        // Create xofapi object.
        hr = DirectXFileCreate(&pxofapi);
        if (FAILED(hr))
            break;

        // Registe templates for d3drm.
        hr = pxofapi->RegisterTemplates((LPVOID)D3DRM_XTEMPLATES,
                                        D3DRM_XTEMPLATE_BYTES);
        if (FAILED(hr))
            break;

        // Create enum object.
        hr = pxofapi->CreateEnumObject((LPVOID)szFilename,
                                       DXFILELOAD_FROMFILE,
                                       &pxofenum);
        if (FAILED(hr))
            break;

        // Enumerate top level objects.
        // Top level objects are always data object.
        LPDIRECTXFILEDATA pxofobj;
        while (SUCCEEDED(pxofenum->GetNextDataObject(&pxofobj))) {
            const GUID *type;

            // Get the type of the object
            hr = pxofobj->GetType(&type);
            if (FAILED(hr))
                break;

            // Display the type
            if (*type == TID_DXFILEHeader)
                cout << "Header\n";
            else if (*type == TID_D3DRMMesh)
                cout << "Mesh\n";

            // Enumerate child objects.
            // Child object can be data, data reference or binary.
            // Use QueryInterface() to find what type of object a child is.
            LPDIRECTXFILEOBJECT pxofChild;
            while (SUCCEEDED(pxofobj->GetNextObject(&pxofChild))) {

                cout << "   Child\n";

                // Query the child for it's FileDtaaReference
                LPDIRECTXFILEDATAREFERENCE pxofdr;
                hr = pxofChild->QueryInterface(IID_IDirectXFileDataReference,
                                               (LPVOID *)&pxofdr);
                if (SUCCEEDED(hr)) {

                    cout << "Data reference.\n";

                    // Resolve data reference.
                    // It's always resolved to a data object.

                    LPDIRECTXFILEDATA pxofdata;

                    hr = pxofdr->Resolve(&pxofdata);
                    if (SUCCEEDED(hr)) {
                        // Do whatever you want with that data object.
                        pxofdata->Release();
                    }

                    pxofdr->Release();
                }

                pxofChild->Release();
            }

            pxofobj->Release();
        }

    } while (FALSE);

    // Clean up any interfaces we still have
    if (pxofenum)
        pxofenum->Release();

    if (pxofapi)
        pxofapi->Release();

    cout << "Done.\n";

    return 0;
}

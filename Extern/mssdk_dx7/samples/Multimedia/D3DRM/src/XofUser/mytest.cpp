//-----------------------------------------------------------------------------
// File: mytest.cpp
//
// Desc: Main program for user data save to .X File sample
//
// Copyright (C) 1998-1999 Microsoft Corporation. All Rights Reserved.
//-----------------------------------------------------------------------------
#include <windows.h>
#include <iostream.h>
#include <objbase.h>
#include "dxfile.h"
#include "mysave.h"

HRESULT save_test(DXFILEFORMAT xFormat);

int main(int argc, char*argv[])
{
    DXFILEFORMAT xFormat = DXFILEFORMAT_TEXT;

    if (argc > 1 &&
        strlen(argv[1]) > 1 &&
        (argv[2][1] == 'b' || argv[2][1] == 'B'))
        xFormat = DXFILEFORMAT_BINARY;

    save_test(xFormat);

    cout << "Done.\n";

    return 0;
}


HRESULT save_test(DXFILEFORMAT xFormat)
{
    XOFSave SaveObj;
    return SaveObj.Run(xFormat);
}

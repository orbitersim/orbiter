//-----------------------------------------------------------------------------
// File: myguids.h
//
// Desc: Classes used for user data
//
// Copyright (C) 1998-1999 Microsoft Corporation. All Rights Reserved.
//-----------------------------------------------------------------------------
#ifndef _MYTYPES_H_
#define _MYTYPES_H_


class TSimple {
public:
    DWORD item1;
    DWORD item2;
    DWORD item3;
};

class TArray {
public:
    DWORD cItems;
    DWORD aItems[1];
};

class TRestricted {
public:
    DWORD item;
};

#endif // _MYTYPES_H_


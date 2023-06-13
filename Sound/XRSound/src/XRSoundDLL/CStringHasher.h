// ==============================================================
// CString hasher class
//
// Copyright (c) 2018-2021 Douglas Beachy
// Licensed under the MIT License
// ==============================================================

#pragma once

#include <atlstr.h>   // for CString
#include <unordered_map>

//=========================================================================
// The following class defines a hash function for string objects.
// Similar to algorithm in http://stackoverflow.com/a/15811185/2347831
//=========================================================================
class CStringHasher
{
public:
    // Returns hashcode for the supplied CString
    size_t operator() (const CString &csKey) const
    {
        size_t hash = 0;
        for (int i = 0; i < csKey.GetLength(); i++)
            hash += (71 * hash + csKey[i]) % 5;

        return hash;
    }

    // Compares two CString objects for equality; returns true if strings match
    bool operator() (const CString &s1, const CString &s2) const
    {
        return (s1 == s2);
    }
};

// ==============================================================
// From the XR Vessel Framework.
//
// Copyright (c) 2006-2021 Douglas Beachy
// Licensed under the MIT license
//
// ConfigFileParserMacros.h
// Defines macros for parsing config files.
// ==============================================================

#pragma once

// convenience macros
// Note: by design, string comparisons are case-insensitive
#define SECTION_MATCHES(str)    (_stricmp(pSection, str) == 0)
#define SECTION_STARTSWITH(str) (_strnicmp(pSection, str, (len = static_cast<int>(strlen(str)))) == 0) 
#define PNAME_MATCHES(str)      (_stricmp(pPropertyName, str) == 0)  
#define PNAME_STARTSWITH(str)   (_strnicmp(pPropertyName, str, (len = static_cast<int>(strlen(str)))) == 0) 
#define VALUE_MATCHES(str)      (_stricmp(pValue, str) == 0)

// Must terminate last byte in the string in case pValue is too long to fit.
// maxLength does NOT include space for the trailing null, so dest must actually be maxLength+1 in size
#define STRNCPY(dest, maxLength) strncpy(dest, pValue, maxLength); if (*pValue == 0) { WriteLog("Value is missing."); return false; } dest[maxLength] = 0; processed = true;
#define CSTRING_CPY(cstr) { LPTSTR buff = cstr.GetBuffer(MAX_VALUE_LENGTH); strncpy(buff, pValue, MAX_VALUE_LENGTH); cstr.ReleaseBuffer(); if (cstr.IsEmpty()) { WriteLog("Value is invalid or missing"); return false; } processed = true; }

#define SSCANF_BOOL(formatStr, a1)  { char c; if (sscanf(pValue, formatStr, &c) < 1) { WriteLog("Value is invalid or missing"); return false; } *a1 = ((c - '0') != 0); processed = true; } /* ASCII 0,1 to true/false */
#define SSCANF1(formatStr, a1)  if (sscanf(pValue, formatStr, a1) < 1) { WriteLog("Value is invalid or missing"); return false; } processed = true;
#define SSCANF2(formatStr, a1, a2)  if (sscanf(pValue, formatStr, a1, a2) < 2) { WriteLog("One or more values are invalid or missing; 2 values required"); return false; } processed = true;
#define SSCANF3(formatStr, a1, a2, a3)  if (sscanf(pValue, formatStr, a1, a2, a3) < 3) { WriteLog("One or more values are invalid or missing; 3 values required"); return false; } processed = true;
#define SSCANF4(formatStr, a1, a2, a3, a4)  if (sscanf(pValue, formatStr, a1, a2, a3, a4) < 4) { WriteLog("One or more values are invalid or missing; 4 values required"); return false; } processed = true;
#define SSCANF5(formatStr, a1, a2, a3, a4, a5)  if (sscanf(pValue, formatStr, a1, a2, a3, a4, a5) < 5) { WriteLog("One or more values are invalid or missing; 5 values required"); return false; } processed = true;

// special macros
#define SSCANF4_OPT(formatStr, a1, a2, a3, a4)  sscanf(pValue, formatStr, a1, a2, a3, a4); processed = true;

#define VALIDATE_INT(valPtr, min, max, def)    if (ValidateInt(*(valPtr), min, max) == false)    { *(valPtr) = def; return false; }
#define VALIDATE_DOUBLE(valPtr, min, max, def) if (ValidateDouble(*(valPtr), min, max) == false) { *(valPtr) = def; return false; }
#define VALIDATE_FLOAT(valPtr, min, max, def)  if (ValidateFloat(*(valPtr), min, max) == false)  { *(valPtr) = def; return false; }

// convert a #define'd property to a string; see https://stackoverflow.com/questions/16989730/stringification-how-does-it-work
#define TO_STR(s) TO_STR2(s)
#define TO_STR2(s) #s
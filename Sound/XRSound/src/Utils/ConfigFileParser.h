// ==============================================================
// From the XR Vessel Framework
//
// Copyright (c) 2006-2021 Douglas Beachy
// Licensed under the MIT License
//
// ConfigFileParser.h
// Base class to parse a configuration file.
// ==============================================================

#pragma once

#include <Windows.h>
#include <stdio.h>

#include <atlstr.h>		// for CString
#include <fstream>      // for ifstream

const int MAX_LINE_LENGTH = 1024;
const int MAX_NAME_LENGTH = 256;
const int MAX_VALUE_LENGTH = (MAX_LINE_LENGTH - MAX_NAME_LENGTH - 1);

class ConfigFileParser
{
public:
    ConfigFileParser(const char *pDefaultFilename, const char *pLogFilename);
    virtual ~ConfigFileParser();
    
    virtual const CString &GetLogPrefix() const { return m_logPrefix; }

    virtual bool ParseFile(const char *pFilename = nullptr);  // main parse method; if null, parses the default filename (GetDefaultFilename())
    bool ParseFailed() const { return m_parseFailed; };
    const char *GetDefaultFilename() const { return m_csDefaultFilename; }   // e.g,. "Foo\Bar.cfg"
    const char *GetOverrideFilename() const { return m_csOverrideFilename; } // e.g,. "Config\XR2-foobar1.xrcfg"; may be empty
    const char *GetConfigFilenames() const { return m_csConfigFilenames; };  // cosmetic string: "Config\XR2RavenstarPrefs.cfg + Config\XR2-foobar.xrcfg"
    
    // returns filename currently being parsed
    const char *GetCurrentFilename(const bool bParsingOverrideFile) const
    {
        if (bParsingOverrideFile)
            return GetOverrideFilename();
        else
            return GetDefaultFilename();
    }

    // e.g., "XR2-01" shows "01.06.2018 15:58:20.064 - [XR2-01] Some important message..." in the log
    // if empty, no prefix will be shown.
    void SetLogPrefix(const char *pPrefix) { m_logPrefix = pPrefix; }

    void WriteLog(const char *pMsg) const;

    //
    // Static utility methods
    //

    static void TrimString(char *pStr);

    // Returns true if the supplied file exists and is readable
    static bool IsFileReadable(const char *pFilename)
    {
        if (!pFilename || !*pFilename)
            return false;

        std::ifstream file(pFilename);
        return file.good();
    }

protected:
    bool ValidateInt(const int value, const int min, const int max) const;
    bool ValidateBool(const int value) const { return ValidateInt(value, 0, 1); };
    bool ValidateDouble(const double value, const double min, const double max) const;
    bool ValidateFloat(const float value, const float min, const float max) const;

    // the subclass must implement this method
    virtual bool ParseLine(const char *pSection, const char *pName, const char *pValue, const bool bParsingOverrideFile) = 0;

    bool m_parseFailed;     // true if parse failed, false if it succeeded
    FILE *m_pLogFile;
    CString m_csDefaultFilename;          // e.g,. "Config\XR2RavenstarPrefs.cfg"
    char m_buffer[MAX_LINE_LENGTH];
    char m_section[256];                  // value between brackets in [SECTION]; changes as each new section is encountered
    char m_parsedName[MAX_NAME_LENGTH];   // left of '='
    char m_parsedValue[MAX_VALUE_LENGTH]; // right of '='

    CString m_csOverrideFilename;   // e.g,. "Config\XR2-foobar1.xrcfg"; may be empty
    CString m_csConfigFilenames;    // cosmetic string: "Config\XR2RavenstarPrefs.cfg + Config\XR2-foobar.xrcfg"

private:
    CString m_logPrefix;
};

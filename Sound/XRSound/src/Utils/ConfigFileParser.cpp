// ==============================================================
// From the XR Vessel Framework
// 
// Copyright (c) 2006-2021 Douglas Beachy
// Licensed under the MIT License
//
// ConfigFileParser.cpp
// Abstract base class to parse a configuration file.
// Blank lines and lines beginning with "#" are ignored.
//
// Format is:
//
// [SECTION]
// name=value [,value2,value3,...]
//
// [SECTION-2]
// ...
// ==============================================================

#include "ConfigFileParser.h"

#include <Shlwapi.h>   // for PathFileExists
#include <string.h>
#include <atlstr.h>

// Constructor
// pDefaultFilename = path to default config file; may be relative to Orbiter root or absolute
// pLogFilename = path to optional (but highly recommended) log file; may be null
ConfigFileParser::ConfigFileParser(const char *pDefaultFilename, const char *pLogFilename) :
    m_pLogFile(nullptr), m_parseFailed(false) 
{
    m_csDefaultFilename = pDefaultFilename;
    
    if (pLogFilename != nullptr)
    {
        // open the log file in APPEND and SHARED mode so we can share it between multiple ship instances
        m_pLogFile = fopen(pLogFilename, "a+t");
        if (m_pLogFile == nullptr)
        {
            char temp[256];
            sprintf(temp, "Error opening log file '%s' for writing; attempting to continue", pLogFilename);
            MessageBox(nullptr, temp, "XR Framework Warning", MB_OK | MB_SETFOREGROUND);
        }
    }
}

// Destructor
ConfigFileParser::~ConfigFileParser()
{
    // clean up and close our logfile
    fclose(m_pLogFile);
}

//
// Main config file parse method.  If you parse multiple files via the same parser object, newer values
// will overwite any previous values. This allows you to create optional "override" configuration files as well
// as the normal ("default") file.
//
// pFilename: (optional) path\filename to parse.  If missing or nullptr, parses the default config filename that was passed to the constructor.
//
// Returns: true on success, false if file does not exist or I/O error or parse error occurs
bool ConfigFileParser::ParseFile(const char *pFilename)
{
    if (pFilename == nullptr)
        pFilename = GetDefaultFilename();

    const bool bParsingOverrideFile = (_stricmp(pFilename, GetDefaultFilename()) != 0);  // true if we are parsing an override file

    static char temp[256]; // reused for messages

    // open the config file
    sprintf(temp, "Parsing config file '%s'", pFilename);
    WriteLog(temp);

    FILE *pFile = fopen(pFilename, "rt");

    if (pFile == nullptr)
    {
        sprintf(temp, "ERROR: fopen failed for '%s'; GetLastError=0x%X", pFilename, GetLastError());
        WriteLog(temp);
        m_parseFailed = true;
        return false;       // could not open file
    }
    
    // read line loop
    bool retVal = true;     // assume success
    for (int lineNumber = 1;; lineNumber++)
    {
        if (fgets(m_buffer, MAX_LINE_LENGTH, pFile) == nullptr)
        {
            if (feof(pFile) == false)    // not at EOF yet?
                retVal = false;          // error parsing file

            // we reached EOF; stop parsing
            break;
        }

        //
        // We have a line -- parse it
        //
        TrimString(m_buffer);
        
        // check for a blank line or comment line
        if ((*m_buffer == 0) || (*m_buffer == '#'))
            continue;   // skip this line

        // check for a new section marker
        if (*m_buffer == '[')
        {
            bool foundClosingBracket = false;
            // copy all characters up to the trailing ']'
            int i;
            for (i=0; i < sizeof(m_section)-1; i++)
            {
                const char c = m_buffer[i+1];   // skip leading '['
                if (c == 0)
                    break;      // end of string, but no bracket found!

                if (c == ']')
                {
                    foundClosingBracket = true;
                    break;
                }
                
                // normal char; copy it 
                m_section[i] = c;
            }

            m_section[i] = 0;       // terminate section string
        }
        else    // this is a normal line
        {
            // find the '=' separating name from value
            const char *pEquals = strchr(m_buffer, '=');
            if (pEquals == nullptr)
            {
                sprintf(temp, "Error parsing line #%d of file '%s': missing '=' character.  Line='%s'", lineNumber, pFilename, m_buffer);
                WriteLog(temp);
                retVal = false;
                continue;  // do NOT stop parsing; just skip to the next line
            }

            // validate name length
            const int nameLength = static_cast<int>((pEquals - m_buffer));    // # of chars in parsed name
            if (nameLength > MAX_NAME_LENGTH)
            {
                sprintf(temp, "Error parsing line #%d of file '%s': name parameter too long (exceeds %d characters).  Line='%s'", 
                    lineNumber, pFilename, MAX_NAME_LENGTH, m_buffer);
                WriteLog(temp);
                retVal = false;
                continue;  // do NOT stop parsing; just skip to the next line
            }

            // validate value length
            const char *pValue = pEquals+1;   // skip '='
            const int valueLength = static_cast<int>(strlen(pValue));
            if (valueLength > MAX_VALUE_LENGTH)
            {
                sprintf(temp, "Error parsing line #%d of file '%s': value parameter too long (exceeds %d characters).  Line='%s'", 
                    lineNumber, pFilename, MAX_VALUE_LENGTH, m_buffer);
                WriteLog(temp);
                retVal = false;
                continue;  // do NOT stop parsing; just skip to the next line
            }

            // copy parsed name and value strings so they can be sent to the subclass for processing
            // the subclass is free to modify these buffers since they are overwritten for each line
            memmove(m_parsedName, m_buffer, nameLength);    // copy name string
            m_parsedName[nameLength] = 0;   // terminate string
            strcpy(m_parsedValue, pValue);  // copy value string

            // trim leading and trailing whitespace from the name and value
            TrimString(m_parsedName);
            TrimString(m_parsedValue);

            // invoke the subclass to parse these values
            if (ParseLine(m_section, m_parsedName, m_parsedValue, bParsingOverrideFile) == false)
            {
                sprintf(temp, "Name/Value error parsing line #%d of file '%s': Line='%s'.  Check the above log message for details.", 
                    lineNumber, pFilename, m_buffer);
                WriteLog(temp);
                retVal = false;
                continue;  // do NOT stop parsing; just skip to the next line
            }
            // parse was successful; return to the top of the loop to parse the next line
        }
    }

    // reset the active section to empty
    *m_section = 0;

    // clean up and exit
    fclose(pFile);
    if (retVal)     // success?
    {
        sprintf(temp, "Successfully parsed configuration file '%s'", pFilename);
        WriteLog(temp);
    }
    else
        m_parseFailed = true;
    
    return retVal;
}

//
// Static utility method to remove all whitespace (nonprintable characters) from the beginning and end of a string
//
// pStr = string to be trimmed
//
void ConfigFileParser::TrimString(char *pStr) 
{
    char * const pOrgStart = pStr;   // original start of string
    char *pStart = pStr;  // start of string; chases pStr until 1st non-whitespace char found

    // pass #1: delete all leading whitespace
    bool trim = true;
    for (char *pOut = pStr;; pOut++, pStr++)
    {
        char c = *pStr;
        if (c == 0)
            break;  // end of string

        if ((c > 32) && (c < 127))
        {
            // first real character reached, so stop timming whitespace
            trim = false;  
        }
       
        if (trim)       // are we skipping this byte?
            pStart++;   // update new start of string ptr
        else            // NOT trimming this byte
            *pOut = c;  // write to output
    }

    // pass #2: delete all trailing whitespace
    // pStr == end of string (on nullptr byte)
    // iterate in reverse
    for (;;)
    {
        if (pStr == pStart)
        {
            // since we reached the start of the string, we found NO non-whitespace characters from the end, so the string was ALL whitespace
            *pStart = 0;    // empty
            break;
        }

        pStr--;   // bump to previous byte

        char c = *pStr;
        if ((c > 32) && (c < 127))
        {
            // found valid end-of-string (non-whitespace)
            pStr[1] = 0;  // terminate it after this character
            break;
        }
    }

    // final step: shift string left to delete trimmed whitespace
    strcpy(pOrgStart, pStart);
}


// log a message
void ConfigFileParser::WriteLog(const char *pMsg) const
{
    // nothing to do if msg is null or if logging disabled
    if ((pMsg == nullptr) || (m_pLogFile == nullptr)) 
        return;

    CStringA csMsg;
    // get and format the current time
    SYSTEMTIME st;
    GetLocalTime(&st);
    CString csPrefix;
    if (!GetLogPrefix().IsEmpty())
        csPrefix.Format("[%s] ", static_cast<const char *>(GetLogPrefix()));

    csMsg.Format("%02d.%02d.%04d %02d:%02d:%02d.%03d - %s%s\n", 
        st.wMonth, st.wDay, st.wYear, 
        st.wHour, st.wMinute, st.wSecond, st.wMilliseconds,
        static_cast<const char *>(csPrefix), pMsg);

    // no point in checking for error here
    OutputDebugString(csMsg);   // send to debug console
    fwrite(csMsg, 1, csMsg.GetLength(), m_pLogFile);

    // flush to disk in case we crash or are terminated
    fflush(m_pLogFile);
}

// logs an error and returns false if the supplied value is out-of-range
bool ConfigFileParser::ValidateInt(const int value, const int min, const int max) const
{
    bool retVal = true; // assume success

    char temp[256];
    if (value < min)
    {
        sprintf(temp, "Integer value '%d' is below minimum value of '%d'", value, min);
        WriteLog(temp);
        retVal = false;
    }
    else if (value > max)
    {
        sprintf(temp, "Integer value '%d' is above maximum value of '%d'", value, max);
        WriteLog(temp);
        retVal = false;
    }

    return retVal;
}

// logs an error and returns false if the supplied value is out-of-range
bool ConfigFileParser::ValidateDouble(const double value, const double min, const double max) const
{
    bool retVal = true; // assume success

    char temp[256];
    if (value < min)
    {
        sprintf(temp, "Double value '%lf' is below minimum value of '%lf'", value, min);
        WriteLog(temp);
        retVal = false;
    }
    else if (value > max)
    {
        sprintf(temp, "Double value '%lf' is above maximum value of '%lf'", value, max);
        WriteLog(temp);
        retVal = false;
    }

    return retVal;
}

// logs an error and returns false if the supplied value is out-of-range
bool ConfigFileParser::ValidateFloat(const float value, const float min, const float max) const
{
    bool retVal = true; // assume success

    char temp[256];
    if (value < min)
    {
        sprintf(temp, "Float value '%f' is below minimum value of '%f'", value, min);
        WriteLog(temp);
        retVal = false;
    }
    else if (value > max)
    {
        sprintf(temp, "Float value '%f' is above maximum value of '%f'", value, max);
        WriteLog(temp);
        retVal = false;
    }

    return retVal;
}


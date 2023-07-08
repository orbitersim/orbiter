// ==============================================================
// Parses a tree of files, optionally recursing subdirectories,
// and invoking an abstract callback method for each file and folder.
// 
// Copyright (c) 2018-2021 Douglas Beachy
// Licensed under the MIT License
// ==============================================================

#include "FileList.h"
#include "Orbitersdk.h"   // for oapiRand

// Convenience constructor for when you want to accept all file types
FileList::FileList(const char *pRootPath, const bool bRecurseSubfolders) :
    m_rootPath(pRootPath), m_bRecurseSubfolders(bRecurseSubfolders), m_previousRandomFileIndex(-1)
{
}

// Convenience constructor for when you only need to accept a single file type (e.g., "*.cfg")
FileList::FileList(const char *pRootPath, const bool bRecurseSubfolders, const char *pFileTypeToAccept) :
    FileList(pRootPath, bRecurseSubfolders)
{
    m_fileTypesToAccept.push_back(pFileTypeToAccept);   // copied by value
}

// Normal Constructor
//  pRootPath: base path to scan; may be relative or absolute path
//  bRecurseSubfolders: true to recurse, false to not
//  pFileTypesToAccept: vector of case-insensitive file extensions to accept (e.g., '.cfg', '.flac', etc).  If empty, all files are accepted.
FileList::FileList(const char *pRootPath, const bool bRecurseSubfolders, const vector<CString> &fileTypesToAccept) :
    FileList(pRootPath, bRecurseSubfolders)
{
    m_fileTypesToAccept = fileTypesToAccept;     // copied by value (i.e., it is cloned)
}

// Destructor
FileList::~FileList()
{
}

#define STARTSWITH_DOT(fd)  (*(fd.cFileName) == '.')
#define IS_EMPTY(fd)        ((fd.nFileSizeHigh == 0) && (fd.nFileSizeLow == 0))
#define IS_DIRECTORY(fd)    (fd.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)

// Resursive method to scan a tree of files, invoking clbkFilterNode for each.  
// If clbkFilterNode returns true:
//      For files, *and* the file is not empty, the file node is added to m_allFiles, and
//      For folders, that folder node is recursed into.
// Note: recursionLevel is just here for debugging purposes
void FileList::Scan(const char *pPath, const int recursionLevel)
{
    _ASSERTE(pPath);
    _ASSERTE(*pPath);

    // This code was broken out from XRPayloadClassData::InitializeXRPayloadClassData().

    WIN32_FIND_DATA findFileData;
    char pConfigFilespecWildcard[MAX_PATH];
    sprintf_s(pConfigFilespecWildcard, "%s\\*", pPath);  // iterate through all files and directories

    HANDLE hFind = ::FindFirstFile(pConfigFilespecWildcard, &findFileData);
    if (hFind == INVALID_HANDLE_VALUE)
        return;    // directory is empty (should really never happen because "." and ".." are always present)

    // found at least one file
    goto process_file;   // a little goofy, but that's Windows' goofy FindFirstFile/FindNextFile for you...

    // now loop through all remaining files
    for (;;)
    {
        if (::FindNextFile(hFind, &findFileData) != 0)
        {
        process_file:   // entry point from FirstFirstFile
                        // build the new configdir-relative path
            char pNodeFilespec[MAX_PATH];
            sprintf_s(pNodeFilespec, "%s\\%s", pPath, findFileData.cFileName);  // e.g., "Vessels\XRParts.cfg"

            if (!STARTSWITH_DOT(findFileData))   // file not "." or ".."?
            {
                if (clbkFilterNode(pNodeFilespec, findFileData))
                {
                    // node should be included
                    if (IS_DIRECTORY(findFileData))
                    {
                        // this is a directory, so recurse down into it
                        Scan(pNodeFilespec, recursionLevel + 1);
                    }
                    else if (!IS_EMPTY(findFileData))  // it's a file node; is it not empty?
                    {
                        // it's a file and it's not empty, so add it to our master list of nodes and invoke the callback for subclasses to hook
                        m_allFiles.push_back(pNodeFilespec);
                        clbkProcessFile(pNodeFilespec, findFileData);
                    }
                }
            }
        }  // if (::FindNextFile(hFind, &findFileData) != 0)
        else   // FindNextFile failed
        {
            // check the error; continue parsing next file unless "no more files" reached
            DWORD dwError = GetLastError();
            if (dwError == ERROR_NO_MORE_FILES)
                break;      // all done
                            // else skip this file: fall through and parse the next file
        }
    }  // for (;;)
    FindClose(hFind);
}

// Invoked for each file or folder node found.  The default method here looks at bRecurseSubfolders (for folder nodes) and
// pFileTypesToAccept (for file nodes) to decide whether accept a node or not.
// Subclasses should override this method if they want more advanced filtering.
//
// Returns true if file node should be included or folder should be recursed into, or false if the node should be skipped.
bool FileList::clbkFilterNode(const char *pPathOfNode, const WIN32_FIND_DATA &fd)
{
    if (IS_DIRECTORY(fd))
        return m_bRecurseSubfolders; 

    // it's a file node
    bool bAcceptFile = false;
    if (!m_fileTypesToAccept.empty())
    {
        const char *pFileExtension = strrchr(fd.cFileName, '.');
        if (pFileExtension)     // e.g., ".flac"
        {
            // see if we have a case-insensitive match for this extension in our master list
            for (vector<CString>::const_iterator it = m_fileTypesToAccept.begin(); it != m_fileTypesToAccept.end(); it++)
            {
                if (_stricmp(pFileExtension, *it) == 0)
                {
                    bAcceptFile = true;
                    break;
                }
            }
        }
    }
    else
        bAcceptFile = true;     // accept all file types

    return bAcceptFile;
}

// Callback invoked for non-empty file nodes that passed the clbkFilterNode check; this is here for subclasses to hook.
void FileList::clbkProcessFile(const char *pFilespec, const WIN32_FIND_DATA &fd)
{
    // no-op; this method is for subclasses to use
}

// Returns a random file entry from the list that is not a repeat of the previous one (provided there are at least two files in the list).
// Returns empty string if the list is empty.
const CString FileList::GetRandomFile()
{
    const int fileCount = GetScannedFileCount();

    // sanity checks
    if (fileCount == 0)
        return "";
    else if (fileCount == 1)
        return m_allFiles[0];   // only one file

    // else we have a normal list of at least two entries, so choose one
    int fileIndex;
    do
    {
        fileIndex = (static_cast<int>(oapiRand() * RAND_MAX) % fileCount); // 0..count-1    
    } while (fileIndex == m_previousRandomFileIndex);

    m_previousRandomFileIndex = fileIndex;
    return m_allFiles[fileIndex];
}

// Returns a random file entry from the list that is not a repeat of the previous one (provided there are at least two files in the list).
//   index: 0..GetScannedFileCount()-1
// Returns empty string if the list is empty.
const CString FileList::GetFile(const int index) const
{
    const int fileCount = GetScannedFileCount();

    // sanity checks
    if ((fileCount == 0) || (index < 0) || index >= fileCount)
        return "";  // index out-of-range

    return m_allFiles[index];
}

// Returns the first file in our file list with the specified basename (case-insensitive search), or nullptr if no file found.
const CString *FileList::FindFileWithBasename(const char *pBasename) const
{
    _ASSERTE(pBasename);
    _ASSERTE(*pBasename);

    const CString *pRetVal = nullptr;
    if (pBasename && *pBasename)
    {
        for (vector<CString>::const_iterator it = m_allFiles.begin(); it != m_allFiles.end(); it++)
        {
            const CString &filespec = *it;   // e.g., "foo\bar.flac", "C:\foo\bar.flac", etc.

            // locate the filename portion of the string
            int lastSeparatorIndex = filespec.ReverseFind('\\');
            if (lastSeparatorIndex < 0)
                lastSeparatorIndex = -1;     // we have just a filename ("bar.flac") -- no leading path; adjust so it will start at index 0 below

            // see if the basename part matches what we're looking for
            // Note: it would be faster for very large filelists to use a hashmap, but this is faster for lists of a few hundred files or less
            const char *pCandidateFilename = static_cast<const char *>(filespec) + lastSeparatorIndex + 1;  // point to filename portion of string (skip over "\")
            for (int i=0;; i++)
            {
                if (!pCandidateFilename[i] || (pCandidateFilename[i] == '.'))   // end of filespec string or end of filename portion (if any)?
                {
                    if (pBasename[i] == 0)   // also end of filename we're searching for (i.e., base names are same length)?
                    {
                        return &filespec;   // this resides in our immutable list, so it's safe to return a pointer to it
                    }
                    break;  // not a match (different lengths)
                }

                if (!pBasename[i] || (tolower(pBasename[i]) != tolower(pCandidateFilename[i])))   // end of basename we're searching for OR characters don't match?
                {
                    break;  // not a match
                }
            }
        }
    }

    return nullptr;
}


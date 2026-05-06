// ==============================================================
// Parses a tree of files, optionally recursing subdirectories,
// and invoking an abstract callback method for each file and folder.
// 
// Copyright (c) 2018-2021 Douglas Beachy
// Licensed under the MIT license
// ==============================================================

#pragma once

#include <vector>
#include <string>
using namespace std;

#include <filesystem>
namespace fs = std::filesystem;

class FileList
{
public:
    FileList(const char *pRootPath, const bool bRecurseSubfolders);
    FileList(const char *pRootPath, const bool bRecurseSubfolders, const char *pFileTypeToAccept);
    FileList(const char *pRootPath, const bool bRecurseSubfolders, const vector<std::string> &fileTypesToAccept);
    virtual ~FileList();

    static bool DirectoryExists(const char *pPath)
    {
        std::error_code ec;
        auto status = fs::status(pPath, ec);
     	if(ec) return false;
    	return fs::is_directory(status);
    }

    // Scan (or rescan) file tree.
    // Returns true on succeess, or false if the root path does not exist or is not a directory.
    bool Scan()
    {
        if (!DirectoryExists(m_rootPath.c_str()))
            return false;

        Scan(m_rootPath.c_str(), 0);
        return true;
    }

    // Invoked for each file or folder node found; should return true if file node should be included or folder should be
    // recursed into, or false if the node should be skipped.
    virtual bool clbkFilterNode(const fs::directory_entry &);

    // Callback invoked for non-empty file nodes that passed the clbkFilterNode check; this is here for subclasses to hook.
    virtual void clbkProcessFile(const fs::directory_entry &);

    int GetScannedFileCount() const { return static_cast<int>(m_allFiles.size()); }
    bool IsEmpty() const { return m_allFiles.empty(); }
    const vector<std::string> &GetScannedFilesList() const { return m_allFiles;  }
    const std::string &GetRootPath() const { return m_rootPath; }

    // returns a random file entry from the list that is not a repeat of the previous one (provided there are at least two files in the list).
    const std::string GetRandomFile();

    // returns a file entry from the list at the specified index (0..GetScannedFileCount()-1)
    const std::string GetFile(const int index) const;

    // Returns the first file in the list with the specified basename.
    const std::string *FindFileWithBasename(const char *pBasename) const;

protected:
    void Scan(const char *pPath, const int recursionLevel);

    std::string m_rootPath;
    bool m_bRecurseSubfolders;
    vector<std::string> m_fileTypesToAccept;
    int m_previousRandomFileIndex;  // 0..GetScannedFileCount()-1

    vector<std::string> m_allFiles;     // full path of all files in the tree, starting with pRootPath.
};

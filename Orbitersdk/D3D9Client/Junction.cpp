// ==============================================================
// Junction.cpp
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// Released under GNU General Public License
// Copyright (C) 2012 Peter Schneider (Kuddel)
//
// With a lot of help from Microsoft and some very important ideas
// regarding 'union padding' form Mahmoud Al-Qudsi of NeoSmart
// ==============================================================

#include "Junction.h"
#include "D3D9Util.h"
#include <string>
#include <memory>
#include <vector>

// This construct is used to get the right size independent of the union padding
#define REPARSE_DATA_BUFFER_HEADER_SIZE offsetof(REPARSE_DATA_BUFFER, GenericReparseBuffer)


namespace junction {


// ===========================================================================
// SafeHandle
#pragma pack(push, 1)
	typedef struct _REPARSE_DATA_BUFFER {
		ULONG  ReparseTag;
		USHORT ReparseDataLength;
		USHORT Reserved;
		union {
			struct {
				USHORT SubstituteNameOffset;
				USHORT SubstituteNameLength;
				USHORT PrintNameOffset;
				USHORT PrintNameLength;
				ULONG  Flags;
				WCHAR  PathBuffer[1];
			} SymbolicLinkReparseBuffer;
			struct {
				USHORT SubstituteNameOffset;
				USHORT SubstituteNameLength;
				USHORT PrintNameOffset;
				USHORT PrintNameLength;
				WCHAR  PathBuffer[1];
			} MountPointReparseBuffer;
			struct {
				UCHAR DataBuffer[1];
			} GenericReparseBuffer;
		};
	} REPARSE_DATA_BUFFER;
#pragma pack(pop)


// ===========================================================================
// Junction methods
	bool CreateJunctionPoint(LPCSTR origin, LPCSTR junction)
	{
		char* buffer = new char[_MAX_PATH];

		GetFullPathName(origin, 256, buffer, NULL);

		// Prepend \??\ to path to mark it as not-for-parsing
		// and convert char -> w_char
		std::string str(buffer);
		std::wstring nativeTarget = L"\\??\\" + std::wstring( str.begin(), str.end() );

		delete[] buffer;

		// Make sure there's a trailing slash
		if (nativeTarget[ nativeTarget.length()-1 ] != L'\\') {
			nativeTarget += L'\\';
		}

		//
		// O.K. Now let's fill the REPARSE_DATA_BUFFER
		//
		size_t size = sizeof(REPARSE_DATA_BUFFER) - sizeof(WCHAR) + nativeTarget.length() * sizeof(WCHAR);
		std::vector<BYTE> vec(size, 0);
		REPARSE_DATA_BUFFER* reparseBuffer = (REPARSE_DATA_BUFFER*)&vec[0];

		reparseBuffer->ReparseTag = IO_REPARSE_TAG_MOUNT_POINT;
		reparseBuffer->Reserved = NULL;
		reparseBuffer->MountPointReparseBuffer.SubstituteNameOffset = 0;
		reparseBuffer->MountPointReparseBuffer.SubstituteNameLength = nativeTarget.length() * (int) sizeof(WCHAR);

		// No substitute name, point it outside the bounds of the string
		reparseBuffer->MountPointReparseBuffer.PrintNameOffset = reparseBuffer->MountPointReparseBuffer.SubstituteNameLength + (int) sizeof(WCHAR);
		reparseBuffer->MountPointReparseBuffer.PrintNameLength = 0;

		// Copy the actual string
		memcpy(reparseBuffer->MountPointReparseBuffer.PathBuffer, (LPCTSTR) nativeTarget.c_str(), reparseBuffer->MountPointReparseBuffer.SubstituteNameLength);

		// Set ReparseDataLength to the size of the MountPointReparseBuffer
		// Kind in mind that with the padding for the union (given that SymbolicLinkReparseBuffer is larger),
		// this is NOT equal to sizeof(MountPointReparseBuffer)
		reparseBuffer->ReparseDataLength = sizeof(REPARSE_DATA_BUFFER) - REPARSE_DATA_BUFFER_HEADER_SIZE - sizeof(WCHAR) + reparseBuffer->MountPointReparseBuffer.SubstituteNameLength;

		// Create the junction directory first, we 'convert' it to junction later
		CreateDirectory(junction, NULL);

		// Set the reparse point
		AutoHandle hDir;
		hDir.Handle = CreateFile(junction, GENERIC_WRITE, 0, NULL, OPEN_EXISTING, FILE_FLAG_OPEN_REPARSE_POINT | FILE_FLAG_BACKUP_SEMANTICS, NULL);
		if (hDir.IsInvalid()) {
			return false; // Failed to open directory!
		}

		DWORD bytesReturned = 0; // dummy
		if (!DeviceIoControl(hDir.Handle, FSCTL_SET_REPARSE_POINT, reparseBuffer, (unsigned int) size, NULL, 0, &bytesReturned, NULL)) {
			return false; // Error issuing DeviceIoControl FSCTL_SET_REPARSE_POINT
		}

		return true;
	}


	bool TargetDirectoryExists(LPCSTR path, DWORD attributes/* = 0*/)
	{
		if (attributes == 0) {
			attributes = ::GetFileAttributes(path);
		}
		if (attributes == INVALID_FILE_ATTRIBUTES) {
			return false; // Doesn't exist
		}
		if ((attributes & FILE_ATTRIBUTE_DIRECTORY) != FILE_ATTRIBUTE_DIRECTORY) {
			return false; // Not a directory
		}
		return true;
	}


	bool IsDirectoryJunction(LPCSTR path, DWORD attributes/* = 0*/)
	{
		if (attributes == 0) {
			attributes = ::GetFileAttributes(path);
		}
		if (attributes == INVALID_FILE_ATTRIBUTES) {
			return false; // Doesn't exist
		}
		if ((attributes & (FILE_ATTRIBUTE_DIRECTORY | FILE_ATTRIBUTE_REPARSE_POINT)) != (FILE_ATTRIBUTE_DIRECTORY | FILE_ATTRIBUTE_REPARSE_POINT)) {
			return false; // Not a directory or not a reparse point
		}
		return true;
	}

} // end-of namespace junction

// --- eof ---
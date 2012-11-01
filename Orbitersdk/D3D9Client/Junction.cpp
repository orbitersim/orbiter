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
#include <string>
#include <memory>

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


/**
 * \brief Kind of auto_handle.
 * This simple wrapper acts like 'auto_ptr' but is for HANDLE. It is used to
 * avoid any not-closed HANDLES leaks when the block scope is left (via thrown
 * exception or return e.g.)
 */
struct SafeHandle
{
	HANDLE Handle;

	SafeHandle () {
		Handle = NULL;
	}

	~SafeHandle () {
		ForceClose();
	}

	bool IsInvalid () {
		return Handle == INVALID_HANDLE_VALUE || Handle == NULL;
	}

	void ForceClose()
	{
		if (!IsInvalid()) {
			CloseHandle(Handle);
		}
		Handle = NULL;
	}
};


// ===========================================================================
// Junction methods
	bool CreateJunctionPoint(LPCSTR origin, LPCSTR junction)
	{
		size_t size;
		std::string str; // multi purpose temporary string

		// Get Orbiter root path
		size = GetCurrentDirectory(0,0);
		std::auto_ptr<char> cwd((char*) new char[size]);
		GetCurrentDirectory(size, cwd.get());

		// char -> w_char
		str = cwd.get();
		std::wstring orbiterRoot( str.begin(), str.end() );

		// Prepend \??\ to path to mark it as not-for-parsing
		// and the orbiter root path
		str = origin;
		std::wstring nativeTarget = L"\\??\\" + orbiterRoot
								  + L"\\" + std::wstring( str.begin(), str.end() );

		// Make sure there's a trailing slash
		if (nativeTarget[ nativeTarget.length()-1 ] != L'\\') {
			nativeTarget += L'\\';
		}

		//
		// O.K. Now let's fill the REPARSE_DATA_BUFFER
		//
		size = sizeof(REPARSE_DATA_BUFFER) - sizeof(WCHAR) + nativeTarget.length() * sizeof(WCHAR);
		std::auto_ptr<REPARSE_DATA_BUFFER> reparseBuffer((REPARSE_DATA_BUFFER*) new unsigned char[size]);

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
		SafeHandle hDir;
		hDir.Handle = CreateFile(junction, GENERIC_WRITE, 0, NULL, OPEN_EXISTING, FILE_FLAG_OPEN_REPARSE_POINT | FILE_FLAG_BACKUP_SEMANTICS, NULL);
		if (hDir.IsInvalid()) {
			return false; // Failed to open directory!
		}

		DWORD bytesReturned = 0; // dummy
		if (!DeviceIoControl(hDir.Handle, FSCTL_SET_REPARSE_POINT, reparseBuffer.get(), (unsigned int) size, NULL, 0, &bytesReturned, NULL)) {
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
// Copyright (c) 2026 Gondos
// Licensed under the MIT License

// Virtual filesystem for Obiter
//
// This file provides the API for an overlay system.
// The user can add directories that will be searched when looking for :
//  - Textures
//  - Meshes
//  - Planetary textures
//  - SPICE kernels
//  - Audio files for XRSound
//  - Lua scripts
//  - Vessel configuration files/modules
//  - Scenarios
//  - Celestial backgrounds
// 
// When the feature is enabled, all modifications to a file are done in a working directory.
// New files are created there. Modifying an existing file will first copy the original file
// to the working directory and then modify it.
// 
// A minimal set of wrappers for opening files/streams is provided, along with some helpers
// mimicking the std::filesystem API
// 
// To limit the risk of ABI breaking between different CRT/C++ runtimes, some helpers are inlined/templatized.
// 
// To prevent deleting important files in case of bugs, the remove_all helper tries to only deletes files in
// the working directory. Since its main use is for removing quicksaves, it shouldn't be too much of a limitation.
// 
// The feature is configured from the Orbiter.cfg file :

// Overlay = TRUE
// WorkDir = C:/WorkingDirectory
// 
// OVERLAYS
//   c:/SPICE
//   c:/Orbiter2016
//   c:/xr2
//   .
// END_OVERLAYS
//
// Directories are overlayed in order of declaration, meaning the ones declared last takes precedence of previous ones
// Subdirectories with the same names are merged between overlays.
// UNC paths are not supported (e.g. \\server\share\xxx)
//



#pragma once

#include "OrbiterAPI.h"
#include <functional>
#include <array>

namespace VFS
{
	// To limit dynamic memory allocations, most functions in the VFS API
	// must be provided with a C array to store their result.
	// This class wraps an array and provides protection against buffer overflows when copying data
	// or appending paths with operator/=
	// In most cases it should not have to be explicitly instanciated by the user.
	class FixedBuffer {
	private:
		char * const m_ptr;
		const size_t m_maxlen;
		bool m_valid = true;
	public:
		template<std::size_t N>
		constexpr FixedBuffer(char (&arr)[N]) noexcept :m_ptr(arr), m_maxlen(N) {
			static_assert(N > 1, "Buffer must have a size > 1"); // so dirname can use arr[0] and arr[1] blindly
			m_ptr[0] = '\0';
		}
		constexpr char *data() const noexcept { return m_ptr; }
		constexpr size_t maxlen() const noexcept { return m_maxlen; }
		constexpr bool valid() const noexcept { return m_valid; }

		// copy will return false and invalidate the buffer in case of error.
		// The buffer is cleared to prevent uninitialized data from being used later.
		// The only way to go from an invalid state to a valid one is to copy valid data
		constexpr bool copy(const char *str) {
			if (!str) {
				m_ptr[0] = '\0';
				m_valid = false;
				return false;
			}

			const size_t len = std::strlen(str);
			if (len >= m_maxlen) {
				m_ptr[0] = '\0';
				m_valid = false;
				return false;
			}

			std::memmove(m_ptr, str, len + 1);
			m_valid = true;
			return true;
		}

		// path must not point into the destination buffer
		constexpr FixedBuffer &operator/=(const char *path) {
			if (!m_valid || !path) {
				m_ptr[0] = '\0';
				m_valid = false;
				return *this;
			}

			size_t offset = 0;
			while (offset < m_maxlen && m_ptr[offset] != '\0')
				++offset;

			// Buffer is already full
			if (offset == m_maxlen) {
				m_ptr[0] = '\0';
				m_valid = false;
				return *this;
			}

			const size_t len = std::strlen(path);

			// Do nothing if the additional path is empty
			if (len == 0)
				return *this;

			const bool separator =
				offset > 0 &&
				m_ptr[offset - 1] != '/' &&
				m_ptr[offset - 1] != '\\' &&
				path[0] != '/' &&
				path[0] != '\\';

			const size_t extra = len + (separator ? 1 : 0);

			// Not enough room to copy the path + separator
			if (extra > m_maxlen - 1 - offset) {
				m_ptr[0] = '\0';
				m_valid = false;
				return *this;
			}

			if (separator)
				m_ptr[offset++] = '/';

			std::memmove(m_ptr + offset, path, len);
			m_ptr[offset + len] = '\0';

			return *this;
		}
	};

	// Don't use std::function at the boundary with the core to limit ABI incompatibilities between runtimes
	typedef void (*EnumerateCallback)(const char* path, void* context);
	/**
	 * \brief Enumerate files and directories in a directory.
	 * \param dir directory to enumerate, must be NUL terminated
	 * \param callback function called for every entry found
	 * \param context pointer to custom data which will be passed to callback
	 * \note The enumeration is not recursive
	 * \note Entries are enumerated in lexical order
	 */
	OAPIFUNC void enumerate(const char* dir, EnumerateCallback callback, void* context = nullptr);

	/**
	 * \brief Enumerate files and directories in a directory.
	 * \param dir directory to enumerate, must be NUL terminated
	 * \param callback callable element called for every entry found
	 * \note The enumeration is not recursive
	 * \note Entries are enumerated in lexical order
	 * \note Prefer this enumeration helper if you want to use a lambda as callback
	 * \note Does nothing if dir or callback is NULL
	 */
	template<typename Callable>
	inline void enumerate(const char* dir, Callable&& callback)	{
		using T = typename std::remove_reference<Callable>::type;
		enumerate(dir, [](const char* path, void* context) { (*static_cast<T*>(context))(path); }, &callback);
	}
	/**
	 * \brief Resolve a virtual path to a physical path.
	 * \param dst fixed sized buffer used to store the result
	 * \param path path to resolve, must be NUL terminated
	 * \param modify boolean indicating if the path is being resolved for reading or writing access
	 * \return pointer to dst containing the result
	 * \note The dst parameter is expected to be a C array, the compiler will promote it to a FixedBuffer automatically 
	 * \note The returned pointer points to the original buffer and will therefore have the same lifetime
	 * \note fopen/stream overrides are provided by default, only use this function when using a 3rd party API requiring a file path.
	 */
	OAPIFUNC const char *resolve_path(FixedBuffer dst, const char *path, bool modify = false);
	/**
	 * \brief Check if a file path has a given extension.
	 * \param path path to check, must be NUL terminated
	 * \param ext extension to check against (without the dot, e.g. "scn"), must be NUL terminated
	 * \param casesensitive boolean indicating if it should be a case sensitive check or not
	 * \return result of the check
	 * \note The dst parameter is expected to be a C array, the compiler will promote it to a FixedBuffer automatically 
	 * \note A file without a stem (e.g. .myfile) is considered having no extension
	 * \note Returns NULL if path or ext is NULL
	 */
	OAPIFUNC bool has_extension(const char *path, const char *ext, bool casesensitive = true);
	/**
	 * \brief Check a file path correspond to a regular file.
	 * \param path path to check, must be NUL terminated
	 * \return result of the check
	 * \note A regular file means a normal file (vs a directory or symlink)
	 * \note false will also be returned in case of an access error
	 * \note Returns false if path is NULL
	 */
	OAPIFUNC bool is_regular_file(const char *path);
	/**
	 * \brief Return the base name of a file path (removing the directory part).
	 * \param path file path, must be NUL terminated
	 * \return pointer to the base name
	 * \note The returned pointer points into the original string
	 * \note Returns NULL if path is NULL
	 */
	OAPIFUNC const char *basename(const char *path);
	/**
	 * \brief Return the directory name of a file path.
	 * \param dst fixed sized buffer used to store the result
	 * \param path file path, must be NUL terminated
	 * \return pointer to the directory name
	 * \note The returned pointer points to the original buffer and will therefore have the same lifetime
	 * \note Returns NULL if path is NULL
	 */
	OAPIFUNC const char *dirname(FixedBuffer dst, const char *path);
	/**
	 * \brief Return the stem of a file path.
	 * \param dst fixed sized buffer used to store the result
	 * \param path file path, must be NUL terminated
	 * \return pointer to the stem
	 * \note The returned pointer points to the original buffer and will therefore have the same lifetime
	 * \note Returns NULL if path is NULL
	 */
	OAPIFUNC const char *stem(FixedBuffer dst, const char *path);
	/**
	 * \brief Create a directory.
	 * \param path directory path, must be NUL terminated
	 * \return status of the operation
	 * \note Returns false if path is NULL or the operation failed
	 */
	OAPIFUNC bool create_directory(const char *path);
	/**
	 * \brief Create a directory tree recursively.
	 * \param path directory path, must be NUL terminated
	 * \return status of the operation
	 * \note Returns false if path is NULL or the operation failed
	 */
	OAPIFUNC bool create_directories(const char *path);
	/**
	 * \brief Delete a directory and all its content.
	 * \param path directory to remove, must be NUL terminated
	 * \return status of the operation
	 * \note When the overlay subsystem is enabled, only relative paths are allowed and only files present in the working directory will be deleted.
	 * \note Returns false if path is NULL or the operation failed
	 */
	OAPIFUNC bool remove_all(const char *path);
	/**
	 * \brief Check if a path is a directory.
	 * \param path directory path, must be NUL terminated
	 * \return status of the check
	 * \note Returns false if path is NULL or the operation failed
	 */
	OAPIFUNC bool is_directory(const char *path);
	/**
	 * \brief Check if a path maps to an existing entry.
	 * \param path path to check, must be NUL terminated
	 * \return status of the check
	 * \note Returns false if path is NULL or the operation failed
	 */
	OAPIFUNC bool exists(const char *path);
	/**
	 * \brief Return the size of a file.
	 * \param path file path
	 * \return file size
	 * \note Returns (uintmax_t)(-1) if path is NULL of the operation failed
	 */
	OAPIFUNC std::uintmax_t file_size(const char *path);
	/**
	 * \brief Load a module.
	 * \param path module path, must be NUL terminated
	 * \return module handle
	 * \note Returns NULL if path is NULL of the operation failed
	 */
	OAPIFUNC void *LoadModule(const char *path);

	/**
	 * \brief fopen override.
	 * \param path file path, must be NUL terminated
	 * \param mode access mode, must be NUL terminated
	 * \return file handle
	 * \note Returns NULL if path is NULL of the operation failed
	 * \note On Windows, the file is opened in non exclusive access mode
	 */
	inline FILE* fopen(const char *path, const char* mode)
	{
		if(!mode) return NULL;

		char rpath[MAX_PATH];

		const bool modify = (strpbrk(mode, "wa+") != nullptr);

		if (!resolve_path(rpath, path, modify))
			return nullptr;

#ifdef _WIN32
		// Open file without exclusive access so we can e.g.
		// open Orbiter.log while the program is running
		return _fsopen(rpath, mode, _SH_DENYNO);
#else
		return std::fopen(rpath, mode);
#endif
	}

	/**
	 * \brief fopen_s override.
	 * \param f pointer to FILE *
	 * \param path file path, must be NUL terminated
	 * \param mode access mode, must be NUL terminated
	 * \return file handle
	 * \note Returns ENOENT if path or mode is NULL
	 */
	inline errno_t fopen_s(FILE **f, const char *path, const char *mode)
	{
		if(!f || !mode) return ENOENT;
		*f = nullptr;
		const bool modify = (strpbrk(mode, "wa+") != nullptr);

		char rpath[MAX_PATH];
		if (!resolve_path(rpath, path, modify))
			return ENOENT;
		
		return ::fopen_s(f, rpath, mode);
	}

	// Helpers for fstream constructors
	struct ResolvedPath
    {
        char path[MAX_PATH]{};
    };
    inline ResolvedPath ResolvePath(const char* path, bool modify)
    {
        ResolvedPath result;
		resolve_path(result.path, path, modify);
        return result;
    }

	/**
	 * \brief ifstream override.
	 */
	class ifstream : public std::ifstream
	{
	public:
		ifstream() = default;
		ifstream(const char *path, std::ios_base::openmode mode = std::ios_base::in):std::ifstream(ResolvePath(path, false).path, mode) {}
		void open(const char *path, std::ios_base::openmode mode = std::ios_base::in)
		{
			char rpath[MAX_PATH];
			if(resolve_path(rpath, path)) {
				std::ifstream::open(rpath, mode);
			} else {
				setstate(std::ios_base::failbit);
			}
		}

	};

	/**
	 * \brief ofstream override.
	 */
	class ofstream : public std::ofstream
	{
	public:
		ofstream() = default;
		ofstream(const char *path, std::ios_base::openmode mode = std::ios_base::out):std::ofstream(ResolvePath(path, true).path, mode) {}
		void open(const char *path, std::ios_base::openmode mode = std::ios_base::out)
		{
			char rpath[MAX_PATH];
			if(resolve_path(rpath, path, true)) {
				std::ofstream::open(rpath, mode);
			} else {
				setstate(std::ios_base::failbit);
			}
		}
	};
};

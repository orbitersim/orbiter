// Copyright (c) 2026 Gondos
// Licensed under the MIT License

#define STRICT 1
#define OAPI_IMPLEMENTATION

#include "VFSAPI.h"
#include "VFS.h"
#include "Orbiter.h"
#include <filesystem>
#include <list>
#include <string>
#include <set>

// For protocol interceptor
#include <windows.h>
#include <exdisp.h>
#include <mshtml.h>
#include <atlbase.h>
#include <atlcom.h>
#include <urlmon.h>
#include <shlwapi.h>
#include <string>
#include <fstream>
#include <vector>

#pragma comment(lib, "urlmon.lib")
#pragma comment(lib, "shlwapi.lib")

extern Orbiter *g_pOrbiter;

static std::list<std::filesystem::path> s_overlays = { "./" };
static std::filesystem::path s_writePath;
static bool s_enabled = false;
static void RegisterVFSProtocol();

// Get the real path of a file, given a virtual path
// When opening a file for modification, we need to copy it to the write
// directory if it's not already there.
// In case path is a directory, we don't copy its content and only create the directory tree
// so that files can later be created there. This is enough for our need.
// /!\ This is not a sandbox, we don't try to prevent access outside the overlay
// or root installation directory. If path is absolute, the overlay is completely bypassed.
static bool GetRealPath(VFS::FixedBuffer dst, const char *path, bool modify = false)
{
	if(!path) {
		dst.data()[0] = '\0';
		return false;
	}

	if(!s_enabled) {
		return dst.copy(path);
	}

	if(s_writePath.empty()) modify = false;

	std::error_code ec;
	// First check in the write path
	auto wpath = s_writePath / path;
	if(std::filesystem::exists(wpath, ec)) {
		return dst.copy(wpath.string().c_str());
	}
	if (ec) {
		oapiWriteLogV("VFS: error accessing %s", wpath.string().c_str());
		return false;
	}

	// Then check in the different overlays
	for(auto &&overlay: s_overlays) {
		auto rpath = overlay / path;
		if(std::filesystem::exists(rpath, ec)) {
			if(modify) {
				// Copy in the work dir if it's a file. Don't bother with special files, Orbiter doesn't make use of them
				const bool is_file = std::filesystem::is_regular_file(rpath, ec);
				if (ec) {
					oapiWriteLogV("VFS: error accessing %s", rpath.string().c_str());
					return false;
				}
				if(is_file) {
					std::filesystem::create_directories(wpath.parent_path(), ec);
					if(ec) {
						oapiWriteLogV("VFS: error creating directory %s", wpath.string().c_str());
						return false;
					}
					std::filesystem::copy(rpath, wpath, ec);
					if(ec) {
						oapiWriteLogV("VFS: error copying %s to %s", rpath.string().c_str(), wpath.string().c_str());
						return false;
					}
				} else { // directory
					std::filesystem::create_directories(wpath, ec);
					if(ec) {
						oapiWriteLogV("VFS: error creating directory %s", wpath.string().c_str());
						return false;
					}
				}
				return dst.copy(wpath.string().c_str());
			} else {
				return dst.copy(rpath.string().c_str());
			}
		} else {
			if (ec) {
				oapiWriteLogV("VFS: error accessing %s", rpath.string().c_str());
				return false;
			}
		}
	}

	// Not found - return the write path if resolving a write access in case we want to create the entry there later
	if(modify) {
		std::filesystem::create_directories(wpath.parent_path(), ec);
		if(ec) {
			oapiWriteLogV("VFS: error creating directory %s", wpath.string().c_str());
			return false;
		}
		return dst.copy(wpath.string().c_str());
	}

	// Not found anywhere and read access -> return original path
	return dst.copy(path);
}

static const char *last_separator(const char *path)
{
	const char *backslash = strrchr(path, '\\');
	const char *slash = strrchr(path, '/');

	if(!backslash) return slash;
	if(!slash) return backslash;

	return backslash > slash ? backslash : slash;
}

static inline bool ends_with(std::string_view str, std::string_view suffix)
{
    return str.size() >= suffix.size() && str.compare(str.size() - suffix.size(), suffix.size(), suffix) == 0;
}

namespace VFS
{
	static bool AddOverlay(const char *path);
	static bool SetWriteDirectory(const char *path);


	void InitVFS()
	{
		std::error_code ec;
		Config *cfg = g_pOrbiter->Cfg();
		CFG_DIRPRM &CfgDirPrm = cfg->CfgDirPrm;
		if(!CfgDirPrm.bOverlay) {
			oapiWriteLog("VFS: virtual file system disabled");
			s_enabled = false;
			return;
		}

		oapiWriteLog("VFS: initializing virtual file system");

		for(auto &overlay: CfgDirPrm.Overlays) {
			// Recurse one deep for xxx/* paths
			if(ends_with(overlay, "/*")) {
				char dir[MAX_PATH];
				if(dirname(dir, overlay.c_str())) {
					std::filesystem::directory_iterator it(dir, ec);
					const std::filesystem::directory_iterator end;

					while (!ec && it != end) {
						if(std::filesystem::is_directory(*it, ec)) {
							auto &entry = *it;
							if(AddOverlay(entry.path().string().c_str())) {
								oapiWriteLogV("VFS: overlay on %s", entry.path().string().c_str());
							} else {
								oapiWriteLogV("VFS: overlay failed on %s", entry.path().string().c_str());
							}
						} else {
							// don't log errors if there's a mix of files/sub directories in the directory
							//oapiWriteLogV("VFS: overlay failed on %s", entry.path().string().c_str());
						}
						it.increment(ec);
					}
					if (ec) {
						oapiWriteLogV("VFS: error enumerating %s", dir);
					}
				} else {
					oapiWriteLogV("VFS: overlay failed on %s", dir);
				}
			} else {
				if(std::filesystem::is_directory(overlay, ec)) {
					if(AddOverlay(overlay.c_str())) {
						oapiWriteLogV("VFS: overlay on %s", overlay.c_str());
					} else {
						oapiWriteLogV("VFS: overlay failed on %s", overlay.c_str());
					}
				} else {
					oapiWriteLogV("VFS: overlay failed on %s", overlay.c_str());
				}
			}
		}

		const char *workdir = CfgDirPrm.WorkDir.c_str();
		if(SetWriteDirectory(workdir)) {
			oapiWriteLogV("VFS: Working directory: %s", workdir);
			s_enabled = true;
		} else {
			oapiWriteLogV("VFS: Failed to set %s as working directory", workdir);
			// Disabling overlay if there was an error setting up the working directory
			s_enabled = false;
		}

		if(s_enabled) {
			RegisterVFSProtocol();
			oapiWriteLogV("VFS: PATH set to %s", getenv("PATH"));
		}
		return;
	}

	std::string GetWritePath()
	{
		return s_writePath.string();
	}

	bool AddOverlay(const char *path)
	{
		// Add the module path to PATH so that LoadLibrary can find DLLs in there
		char modpath[MAX_PATH];
		FixedBuffer fb(modpath);
		fb.copy(path);
		fb /= "Modules";
		if(!fb.valid()) {
			oapiWriteLogV("VFS: overlay path too long (%s)", path);
			return false;
		}

		char *ppath = getenv("PATH");
		std::string newpath;
		if (ppath) {
			newpath = std::string("PATH=")+modpath+";"+ppath;
		} else {
			newpath = std::string("PATH=")+modpath;
		}
		if(_putenv (newpath.c_str()) != 0) {
			oapiWriteLogV("VFS: failed updating PATH (%s)", newpath.c_str());
			return false;
		}

		s_overlays.push_front(path);

		return true;
	}

	bool SetWriteDirectory(const char *path)
	{
		char modpath[MAX_PATH];
		FixedBuffer fb(modpath);
		fb.copy(path);
		fb /= "Modules";
		if(!fb.valid()) {
			oapiWriteLogV("VFS: write path too long (%s)", path);
			return false;
		}

		std::error_code ec;
		std::filesystem::create_directories(path, ec);
		if(ec) {
			oapiWriteLogV("VFS: error creating write path (%s)", path);
			return false;
		}

		// Update the path even though DLLs should never end up here...
		char *ppath = getenv("PATH");
		std::string newpath;
		if (ppath) {
			newpath = std::string("PATH=")+modpath+";"+ppath;
		} else {
			newpath = std::string("PATH=")+modpath;
		}
		if(_putenv (newpath.c_str()) != 0) {
			oapiWriteLogV("VFS: failed updating PATH (%s)", newpath.c_str());
			return false;
		}

		s_writePath = path;

		return true;
	}

	DLLEXPORT bool is_directory(const char *path)
	{
		char rpath[MAX_PATH];
		if(!GetRealPath(rpath, path)) return false;
		std::error_code ec;
		return std::filesystem::is_directory(rpath, ec);
	}

	DLLEXPORT bool exists(const char *path)
	{
		char rpath[MAX_PATH];
		if(!GetRealPath(rpath, path)) return false;
		std::error_code ec;
		return std::filesystem::exists(rpath, ec);
	}

	DLLEXPORT std::uintmax_t file_size(const char *path)
	{
		char rpath[MAX_PATH];
		if(!GetRealPath(rpath, path)) return static_cast<uintmax_t>(-1);
		std::error_code ec;
		return std::filesystem::file_size(rpath, ec);
	}

	DLLEXPORT void enumerate(const char* dir, EnumerateCallback callback, void* context)
	{
		if(!dir || !callback) return;
		if(dir[0] == '\0') return;
		std::error_code ec;
		std::set<std::string> mergednames;
		std::string sdir = std::string(dir) + "/";

		auto add_entries = [&](const std::filesystem::path &path)
		{
			std::error_code ec;
			std::filesystem::directory_iterator it(path, ec);
			const std::filesystem::directory_iterator end;

			if (ec) {
				// Missing or inaccessible directory: skip this layer.
				return;
			}

			while (it != end) {
				mergednames.insert(sdir + it->path().filename().string());

				it.increment(ec);
				if (ec) {
					// Stop enumerating this layer if an entry cannot be read.
					break;
				}
			}
		};

		if (!s_writePath.empty()) {
			add_entries(s_writePath / dir);
		}

		for (const auto &overlay : s_overlays) {
			add_entries(overlay / dir);
		}

		for (const auto &name : mergednames) {
			callback(name.c_str(), context);
		}
	}

	DLLEXPORT bool create_directory(const char *path)
	{
		char rpath[MAX_PATH];
		if(!GetRealPath(rpath, path, true)) return false;
		std::error_code ec;
		return std::filesystem::create_directory(rpath, ec);
	}

	DLLEXPORT bool create_directories(const char *path)
	{
		char rpath[MAX_PATH];
		if(!GetRealPath(rpath, path, true)) return false;
		std::error_code ec;
		return std::filesystem::create_directories(rpath, ec);
	}

	DLLEXPORT void *LoadModule(const char *path) {
		char rpath[MAX_PATH];
		if(!GetRealPath(rpath, path)) return NULL;
		return LoadLibrary(rpath);
	}

	// As a safety net, only allow removing files from the working directory when the overlay is enabled
	// This is not fool proof, if a reparse point or symlink is used in the work dir, it may cause issues
	DLLEXPORT bool remove_all(const char* path)
	{
		if(!s_enabled) {
			if(!path) return false;
			std::error_code ec;
			std::filesystem::remove_all(path, ec);
			return !ec;
		}

		if (!path || !*path || s_writePath.empty())
			return false;

		std::filesystem::path rel(path);

		// Reject absolute paths and explicit traversal.
		if (rel.is_absolute() || rel.has_root_path())
			return false;

		rel = rel.lexically_normal();

		if (rel.empty() || rel == "." || rel == ".." ||
			*rel.begin() == "..")
			return false;

		const auto target = (s_writePath / rel).lexically_normal();
		const auto root = std::filesystem::absolute(s_writePath).lexically_normal();

		// Ensure the target is strictly inside the write directory.
		const auto absTarget = std::filesystem::absolute(target).lexically_normal();
		auto rootIt = root.begin();
		auto targetIt = absTarget.begin();

		for (; rootIt != root.end(); ++rootIt, ++targetIt) {
			if (targetIt == absTarget.end() || *rootIt != *targetIt)
				return false;
		}

		if (targetIt == absTarget.end())
			return false; // Never delete the Work root itself.

		std::error_code ec;
		std::filesystem::remove_all(absTarget, ec);
		return !ec;
	}

	DLLEXPORT bool is_regular_file(const char *path)
	{
		char rpath[MAX_PATH];
		if(!GetRealPath(rpath, path)) return false;
		std::error_code ec;
		return std::filesystem::is_regular_file(rpath, ec);
	}

	DLLEXPORT const char *dirname(FixedBuffer dst, const char *path)
	{
		if(!dst.copy(path)) return NULL;

		char *p = dst.data();
		char *sep = (char*)last_separator(p);

		if (!sep) {
			// No directory component
			p[0] = '.';
			p[1] = '\0';
		} else {
			// Preserve the root separator
			if (sep == p) {
				sep[1] = '\0';
			} else {
				// Handle drive roots, e.g. C:\file.txt
				if (sep == p + 2 && p[1] == ':') {
					sep[1] = '\0';
				} else {
					*sep = '\0';
				}
			}
		}

		return p;
	}

	DLLEXPORT const char* stem(FixedBuffer dst, const char* path)
	{
		if (!dst.copy(basename(path)))
			return NULL;

		char* name = dst.data();
		char* ext = strrchr(name, '.');

		// A leading dot alone doesn't count as an extension.
		if (ext && ext != name)
			*ext = '\0';

		return name;
	}

	DLLEXPORT bool has_extension(const char* path, const char* extension, bool casesensitive)
	{
		if (!path || !extension)
			return false;

		const char* name = basename(path);
		const char* dot = strrchr(name, '.');

		// No extension, or a leading dot only (e.g. ".profile").
		if (!dot || dot == name)
			return *extension == '\0';

		const char* ext = dot + 1;

		return casesensitive ? strcmp(ext, extension) == 0 : _stricmp(ext, extension) == 0;
	}

	DLLEXPORT const char *basename(const char *path)
	{
		if(!path) return nullptr;
		const char *lsep = last_separator(path);
		if(!lsep)
			return path;
		return lsep + 1;
	}

	DLLEXPORT const char *resolve_path(FixedBuffer dst, const char *path, bool modify)
	{
		if(!GetRealPath(dst, path, modify)) return NULL;
		return dst.data();
	}
};

//////////////////////////////////////////////////////////////////
// /!\ COM garbage
// 
// The main window uses a web view to display the scenario description
// It can contain images so we need to provide a way to resolve the overlay path.
// We create a "vfs" protocol to intercept URL combining (see ScenarioTab::ScenarioChanged()). When an <img src=url> tag is used 
// in an html page, it'll call CombineUrl. We "redirect" the url to a vfs:// path that will then be caught by IInternetProtocolRoot::Start
// where we can resolve the overlay.
// Since it'll only be used by the core, no unregistration or fancy COM support is provided.
// Note that almost all of this was coerced out of ChatGPT.
#include <cwchar>
class VFSHandler : public IInternetProtocol, public IInternetProtocolInfo
{
public:
	VFSHandler() : m_pos(0), m_ref(1) {}

	// IUnknown
	STDMETHOD(QueryInterface)(REFIID riid, void** ppv)
	{
		if (!ppv) return E_POINTER;
		if (riid == IID_IUnknown || riid == IID_IInternetProtocol || riid == IID_IInternetProtocolRoot) {
			*ppv = static_cast<IInternetProtocol*>(this);
			AddRef();
			return S_OK;
		}
		if (riid == IID_IInternetProtocolInfo) {
			*ppv = static_cast<IInternetProtocolInfo*>(this);
			AddRef();
			return S_OK;
		}
		*ppv = nullptr;
		return E_NOINTERFACE;
	}

	STDMETHOD_(ULONG, AddRef)()	{ return InterlockedIncrement(&m_ref); }
	STDMETHOD_(ULONG, Release)()
	{
		ULONG ref = InterlockedDecrement(&m_ref);
		if (ref == 0) delete this;
		return ref;
	}

	// IInternetProtocolRoot
	STDMETHOD(Start)(LPCWSTR szUrl, IInternetProtocolSink* pSink, IInternetBindInfo*, DWORD, HANDLE_PTR)
	{
		if (!pSink) return E_POINTER;
		if (!szUrl) return E_INVALIDARG;

		HRESULT hr = S_OK;
		m_sink = pSink;
		m_data.clear();
		m_pos = 0;

		std::wstring url(szUrl);
		wchar_t localFile[MAX_PATH];

		if(url.find(L"vfs://") == 0) {
			std::wstring path = url.substr(6); // skip "vfs://"

			HRESULT hr = UrlUnescapeW(&path[0], NULL, NULL, URL_UNESCAPE_AS_UTF8 | URL_UNESCAPE_INPLACE);
			if (FAILED(hr)) {
				hr = INET_E_INVALID_URL;
				goto fail;
			}
			const int utf8Size = WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS, path.c_str(), -1, NULL, 0, NULL, NULL);
			if (utf8Size == 0) {
                hr = HRESULT_FROM_WIN32(GetLastError());
				goto fail;
            }

			std::string cbuf(utf8Size, '\0');
            if (WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS, path.c_str(), -1, &cbuf[0], utf8Size, NULL, NULL) == 0) {
                hr = HRESULT_FROM_WIN32(GetLastError());
				goto fail;
            }

			char rpath[MAX_PATH];
			if(VFS::resolve_path(rpath, cbuf.c_str())) {
                const int wideSize = MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, rpath, -1, nullptr, 0);
                if (wideSize == 0) {
                    hr = HRESULT_FROM_WIN32(GetLastError());
					goto fail;
                }
				std::wstring localPath(wideSize, L'\0');
				if (MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, rpath, -1, &localPath[0], wideSize) == 0) {
                    hr = HRESULT_FROM_WIN32(GetLastError());
					goto fail;
                }

				std::ifstream file(std::filesystem::path(localPath.c_str()), std::ios::binary);

				if(file) {
					// Load the whole file into memory to simplify the Read implementation
					// We don't expect huge files here
					std::vector<char> buffer((std::istreambuf_iterator<char>(file)), {});
					m_data = std::move(buffer);
					if (file.bad()) {
						m_data.clear();
						hr = E_FAIL;
					} else if (m_data.size() > (std::numeric_limits<ULONG>::max)()) { // Prevent overflow
						m_data.clear();
						hr = INET_E_DOWNLOAD_FAILURE;
					} else {
						hr = S_OK;
					}
				} else {
					hr = INET_E_RESOURCE_NOT_FOUND;
				}
			} else {
				hr = INET_E_RESOURCE_NOT_FOUND;
			}
		} else {
			hr = INET_E_INVALID_URL;
		}
fail:
		if (m_sink) {
			if (SUCCEEDED(hr)) {
				const ULONG size = static_cast<ULONG>(m_data.size());
				m_sink->ReportData(BSCF_FIRSTDATANOTIFICATION, 0, size);
				m_sink->ReportData(BSCF_LASTDATANOTIFICATION, size, size);
			}

			m_sink->ReportResult(hr, 0, 0);
		}
		return hr;
	}

	STDMETHOD(Continue)(PROTOCOLDATA*) { return S_OK; }
	STDMETHOD(Abort)(HRESULT, DWORD) { return S_OK; }
	STDMETHOD(Terminate)(DWORD) { 
		m_sink.Release();
		m_data.clear();
		m_pos = 0;
		return S_OK;
	}
	STDMETHOD(Suspend)() { return E_NOTIMPL; }
	STDMETHOD(Resume)() { return E_NOTIMPL; }

	// IInternetProtocol
	STDMETHOD(Read)(void* pv, ULONG cb, ULONG* pcbRead)
	{
		if (!pcbRead) return E_POINTER;
		*pcbRead = 0;

		if (!pv && cb != 0) return E_POINTER;
		if (cb == 0) return S_OK;

		if (m_pos >= m_data.size())
			return S_FALSE;

		size_t remaining = m_data.size() - m_pos;
		ULONG toCopy = static_cast<ULONG>(
			std::min<size_t>(cb, remaining));

		if (toCopy) {
			memcpy(pv, m_data.data() + m_pos, toCopy);
			m_pos += toCopy;
		}

		*pcbRead = toCopy;
		return S_OK;
	}

	STDMETHOD(Seek)(LARGE_INTEGER, DWORD, ULARGE_INTEGER*) { return E_NOTIMPL; }
	
    // IInternetProtocolInfo
    STDMETHOD(ParseUrl)(
        LPCWSTR pwzUrl,
        PARSEACTION ParseAction,
        DWORD dwParseFlags,
        LPWSTR pwzResult,
        DWORD cchResult,
        DWORD* pcchResult,
        DWORD dwReserved)
    {
        return INET_E_DEFAULT_ACTION;
    }

	HRESULT STDMETHODCALLTYPE CombineUrl(
		LPCWSTR pwzBaseUrl,
		LPCWSTR pwzRelativeUrl,
		DWORD dwCombineFlags,
		LPWSTR pwzResult,
		DWORD cchResult,
		DWORD* pcchResult,
		DWORD dwReserved)
	{
		if (!pwzRelativeUrl)
			return E_POINTER;

		std::wstring relative(pwzRelativeUrl);
		std::filesystem::path path;

		// Already an absolute VFS URL?
		if (_wcsnicmp(relative.c_str(), L"vfs://", 6) == 0) {
			path = std::filesystem::path(relative.substr(6));
		}
		else {
			// Is this an absolute filesystem path?
			const bool absolute =
				// Drive-rooted path: C:\foo or C:/foo
				(relative.size() >= 3 &&
					relative[1] == L':' &&
					(relative[2] == L'/' || relative[2] == L'\\')) ||

				// UNC path: \\server\share
				(relative.size() >= 2 &&
					relative[0] == L'\\' &&
					relative[1] == L'\\') ||

				// Root-relative path
				(!relative.empty() &&
					(relative[0] == L'/' || relative[0] == L'\\'));

			if (absolute) {
				path = std::filesystem::path(relative);
			}
			else {
				// Resolve relative path against the base URL directory.
				std::wstring base = pwzBaseUrl ? pwzBaseUrl : L"vfs://";

				// Remove the VFS URL scheme.
				if (_wcsnicmp(base.c_str(), L"vfs://", 6) == 0)
					base.erase(0, 6);

				std::filesystem::path basePath(base);

				// Use the directory containing the base URL.
				path = basePath.parent_path() / relative;
			}
		}

		// Resolve . and .. and use URL-style separators.
		path = path.lexically_normal();

		std::wstring result = L"vfs://" + path.generic_wstring();

		// Required buffer size, including terminating NUL.
		const size_t required = result.size() + 1;

		if (required > MAXDWORD)
			return E_OUTOFMEMORY;

		if (pcchResult)
			*pcchResult = static_cast<DWORD>(required);

		if (!pwzResult || cchResult < required)
			return HRESULT_FROM_WIN32(ERROR_INSUFFICIENT_BUFFER);

		std::memcpy(pwzResult, result.c_str(), required * sizeof(wchar_t));

		return S_OK;
	}

    STDMETHOD(CompareUrl)(
        LPCWSTR pwzUrl1,
        LPCWSTR pwzUrl2,
        DWORD dwCompareFlags)
    {
        return INET_E_DEFAULT_ACTION;
    }

    STDMETHOD(QueryInfo)(
        LPCWSTR pwzUrl,
        QUERYOPTION QueryOption,
        DWORD dwQueryFlags,
        LPVOID pBuffer,
        DWORD cbBuffer,
        DWORD* pcbBuf,
        DWORD dwReserved)
    {
        return INET_E_DEFAULT_ACTION;
    }	
	
	
	STDMETHOD(LockRequest)(DWORD) { return E_NOTIMPL; }
	STDMETHOD(UnlockRequest)() { return E_NOTIMPL; }

private:
	CComPtr<IInternetProtocolSink> m_sink;
	std::vector<char> m_data;
	size_t m_pos;
	volatile LONG m_ref;
};

class VFSHandlerFactory : public IClassFactory
{
public:
	VFSHandlerFactory() : m_ref(1) {}

	// IUnknown
	STDMETHOD(QueryInterface)(REFIID riid, void** ppv)
	{
		if (!ppv) return E_POINTER;
		if (riid == IID_IUnknown || riid == IID_IClassFactory) { *ppv = this; AddRef(); return S_OK; }
		*ppv = nullptr;
		return E_NOINTERFACE;
	}

	STDMETHOD_(ULONG, AddRef)() { return InterlockedIncrement(&m_ref); }
	STDMETHOD_(ULONG, Release)()
	{
		ULONG ref = InterlockedDecrement(&m_ref);
		if (ref == 0) delete this;
		return ref;
	}

	// IClassFactory
	STDMETHOD(CreateInstance)(IUnknown* pUnkOuter, REFIID riid, void** ppv)
	{
		if (pUnkOuter)
			return CLASS_E_NOAGGREGATION;

		if (!ppv) return E_POINTER;
		*ppv = NULL;

		VFSHandler* obj = new (std::nothrow) VFSHandler();
		if (!obj) return E_OUTOFMEMORY;
		
		HRESULT hr = obj->QueryInterface(riid, ppv);
		obj->Release();
		return hr;
	}

	STDMETHOD(LockServer)(BOOL) { return S_OK; }

private:
	volatile LONG m_ref;
};

void RegisterVFSProtocol()
{
    CComPtr<IInternetSession> session;
    if (SUCCEEDED(CoInternetGetSession(0, &session, 0))) {
        // {1112F5DB-7847-4DA3-BB61-B3570AFB2762}
        static const GUID CLSID_VFSProto = { 0x1112f5db, 0x7847, 0x4da3, { 0xbb, 0x61, 0xb3, 0x57, 0xa, 0xfb, 0x27, 0x62 } };

        session->RegisterNameSpace(new VFSHandlerFactory(), CLSID_VFSProto, L"vfs", 0, nullptr, 0);
    }
}

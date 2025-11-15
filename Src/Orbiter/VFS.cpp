#define STRICT 1
#define OAPI_IMPLEMENTATION

#include "VFSAPI.h"
#include "VFS.h"
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


static std::list<std::filesystem::path> s_overlays = { "./" };
//static std::list<std::string> s_whiteouts;
static std::filesystem::path s_writePath;
static bool s_enabled = false;
static void RegisterVFSProtocol();

// Get the real path of a file, given a virtual path
// When opening a file for modification, we need to copy it to the write
// directory if it's not already there
static void GetRealPath(const char *path, VFS::bounded_path dst, bool modify = false)
{
	if(!s_enabled) {
		dst.copy(path);
		return;
	}

	if(s_writePath.empty()) modify = false;

	std::error_code ec;
	// First check in the write path
	auto wpath = s_writePath / path;
	if(std::filesystem::exists(wpath, ec)) {
		dst.copy(wpath.string().c_str());
		return;
	}

	// The check in the different overlays
	for(auto &&overlay: s_overlays) {
		auto rpath = overlay / path;
		if(std::filesystem::exists(rpath, ec)) {
			if(modify) {
				std::filesystem::create_directories(wpath.parent_path(), ec);
				// Copy if it's a file
				if(std::filesystem::is_regular_file(rpath)) {
					std::filesystem::copy(rpath, wpath, ec);
				}
				dst.copy(wpath.string().c_str());
				return;
			} else {
				dst.copy(rpath.string().c_str());
				return;
			}
		}
	}

	// Not found - return the write path in case we want to create the entry
	if(modify) {
		std::filesystem::create_directories(wpath.parent_path(), ec);
	}
	dst.copy(wpath.string().c_str());
}

static const char *last_separator(const char *path)
{
	const char *backslash = strrchr(path, '\\');
	const char *slash = strrchr(path, '/');

	const char *ret = NULL;
	if(backslash > ret) ret = backslash;
	if(slash > ret) ret = slash;
	return ret;
}


namespace VFS
{
	void InitVFS()
	{
		RegisterVFSProtocol();
		s_enabled = false;

		if(!std::filesystem::is_directory("Addons")) return;

		for(auto &entry: std::filesystem::directory_iterator("Addons")) {
			if(std::filesystem::is_directory(entry)) {
				AddOverlay(entry.path().string().c_str());
				s_enabled = true;
			}
		}
		if(s_enabled)
			SetWritePath("Work");
	}

	void AddOverlay(const char *path)
	{
		s_overlays.push_back(path);

		char modpath[MAX_PATH];
		PathInit(modpath, path);
		PathConcat(modpath, "Modules");
		char *ppath = getenv ("PATH");
		char *cbuf = new char[strlen(ppath)+strlen(modpath)+16];
		if (ppath) {
			::sprintf (cbuf, "PATH=%s;%s", ppath, modpath);
		} else {
			::sprintf (cbuf, "PATH=%s", modpath);
		}
		_putenv (cbuf);
		delete []cbuf;
	}

	void SetWritePath(const char *path)
	{
		s_writePath = path;
		std::filesystem::create_directories(path);

		char modpath[MAX_PATH];
		PathInit(modpath, path);
		PathConcat(modpath, "Modules");
		char *ppath = getenv ("PATH");
		char *cbuf = new char[strlen(ppath)+strlen(modpath)+16];
		if (ppath) {
			::sprintf (cbuf, "PATH=%s;%s", ppath, modpath);
		} else {
			::sprintf (cbuf, "PATH=%s", modpath);
		}
		_putenv (cbuf);
		delete []cbuf;
	}
	
	DLLEXPORT std::string GetWritePath()
	{
		return s_writePath.string();
	}

	void ifstream::open(const char *path, std::ios_base::openmode mode)
	{
		char rpath[MAX_PATH];
		GetRealPath(path, rpath);
		//printf("ifstream f=%s -> %s\n", path, rpath.c_str());
		//fflush(stdout);
		std::ifstream::open(rpath, mode);
	}
	void ofstream::open(const char *path, std::ios_base::openmode mode)
	{
		char rpath[MAX_PATH];
		GetRealPath(path, rpath, true);
		//printf("ofstream f=%s -> %s\n",path, rpath.c_str());
		//fflush(stdout);
		std::ofstream::open(rpath, mode);
	}

	DLLEXPORT FILE *fopen(const char * path, const char *mode)
	{
		bool modify = true;
		// Only modes "r", "rb" and "rt" don't imply file modification
		if(mode[0] == 'r' && (mode[1] == 'b' || mode[1] == 't' || mode[1] == '\0'))
			modify = false;

		char rpath[MAX_PATH];
		GetRealPath(path, rpath, modify);
		//printf("mode=%s f=%s -> %s\n",mode, path, rpath.c_str());
		//fflush(stdout);
		FILE *ret;
#ifdef _WIN32
//		if (fopen_s(&ret, rpath.c_str(), mode)) return NULL;
		// Open file without exclusive access so we can e.g.
		// open Orbiter.log while the program is running
		ret = _fsopen(rpath, mode, _SH_DENYNO );
#else
		ret =  ::fopen(path, mode);
#endif
		return ret;
	}

	DLLEXPORT bool is_directory(const char *path)
	{
		char rpath[MAX_PATH];
		GetRealPath(path, rpath);
		std::error_code ec;
		return std::filesystem::is_directory(rpath, ec );
	}

	DLLEXPORT bool exists(const char *path)
	{
		char rpath[MAX_PATH];
		GetRealPath(path, rpath);
		std::error_code ec;
		return std::filesystem::exists(rpath, ec );
	}

	DLLEXPORT void enumerate(const char *dir, std::function<void(const char *)> callback)
	{
		std::error_code ec;
		std::set<std::string> mergednames;
		std::string sdir = std::string(dir) + "/";

		if(!s_writePath.empty()) {
			auto wpath = s_writePath / dir;
			for (const auto& entry : std::filesystem::directory_iterator(wpath, ec)) {
				mergednames.insert(sdir + entry.path().filename().string());
			}
		}

		for(auto &&overlay: s_overlays) {
			auto rpath = overlay / dir;

			for (const auto& entry : std::filesystem::directory_iterator(rpath, ec)) {
				mergednames.insert(sdir + entry.path().filename().string());
			}
		}

		for(auto &&name: mergednames) {
			callback(name.c_str());
		}
	}

	DLLEXPORT void create_directory(const char *path)
	{
		char rpath[MAX_PATH];
		GetRealPath(path, rpath, true);
		std::filesystem::create_directory(rpath);
	}

	DLLEXPORT void *LoadModule(const char *path) {
		char rpath[MAX_PATH];
		GetRealPath(path, rpath);
		//printf("LoadModules f=%s -> %s\n", path, rpath.c_str());
		//fflush(stdout);

		return LoadLibrary(rpath);
	}

	DLLEXPORT void remove_all(const char *path)
	{
		char rpath[MAX_PATH];
		GetRealPath(path, rpath, true);
		std::filesystem::remove_all(rpath);
	}

	DLLEXPORT bool is_regular_file(const char *path)
	{
		char rpath[MAX_PATH];
		GetRealPath(path, rpath);
		std::error_code ec;
		return std::filesystem::is_regular_file(rpath, ec);
	}


	DLLEXPORT const char *dirname(const char *path, bounded_path dst)
	{
		dst.copy(path);

		char *lsep = (char *)last_separator(dst.data()); // safe because dst is non const

		if(!lsep) {
			lsep = dst.data();
		}

		*lsep = '\0';

		return dst.data();
	}

	DLLEXPORT const char *stem(const char *path, bounded_path dst)
	{
		dst.copy(basename(path));

		char *fext = strrchr(dst.data(), '.');
		if(fext) {
			*fext = '\0';
		}
		return dst.data();
	}

	DLLEXPORT bool has_extension(const char *path, const char *extension)
	{
		const char *fext = strrchr(path, '.');
		if(fext) {
			return !strcmp(fext + 1, extension);
		}
		return *extension == '\0';
	}

	DLLEXPORT const char *basename(const char *path)
	{
		const char *lsep = last_separator(path);
		if(!lsep)
			return path;
		return lsep + 1;
	}

	DLLEXPORT const char *realpath(const char *path, bounded_path dst)
	{
		GetRealPath(path, dst);
		return dst.data();
	}

	DLLEXPORT const char *realpath_ns(const char *path)
	{
		static char rpath[MAX_PATH];
		return realpath(path, rpath);
	}

};

// "vfs" protocol to intercept image loading
class VFSHandler : public IInternetProtocol
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
		m_sink = pSink;
		m_data.clear();
		m_pos = 0;

		std::wstring url(szUrl);
		wchar_t localFile[MAX_PATH];

		if(url.find(L"vfs://") == 0) {
			std::wstring path = url.substr(6); // skip "vfs://"
			char cbuf[MAX_PATH];
			int length = WideCharToMultiByte(CP_UTF8, 0, path.c_str(), -1, cbuf, MAX_PATH, NULL, NULL);
			char rpath[MAX_PATH];
			VFS::realpath(cbuf, rpath);

			MultiByteToWideChar(CP_UTF8, 0, rpath, -1, localFile, MAX_PATH);

			std::ifstream file(localFile, std::ios::binary);
			if(file) {
				std::vector<char> buffer((std::istreambuf_iterator<char>(file)), {});
				m_data.assign(buffer.begin(), buffer.end());
			}
		}

		if(m_sink) {
			m_sink->ReportData(BSCF_FIRSTDATANOTIFICATION, 0, (ULONG)m_data.size());
			m_sink->ReportData(BSCF_LASTDATANOTIFICATION, (ULONG)m_data.size(), (ULONG)m_data.size());
			m_sink->ReportResult(S_OK, 0, 0);
		}

		return S_OK;
	}

	STDMETHOD(Continue)(PROTOCOLDATA*) { return S_OK; }
	STDMETHOD(Abort)(HRESULT, DWORD) { return S_OK; }
	STDMETHOD(Terminate)(DWORD) { return S_OK; }
	STDMETHOD(Suspend)() { return E_NOTIMPL; }
	STDMETHOD(Resume)() { return E_NOTIMPL; }

	// IInternetProtocol
	STDMETHOD(Read)(void* pv, ULONG cb, ULONG* pcbRead)
	{
		if (m_pos >= m_data.size()) { *pcbRead = 0; return S_FALSE; }
		ULONG remaining = (ULONG)(m_data.size() - m_pos);
		ULONG toCopy = std::min(cb, remaining);
		memcpy(pv, m_data.data() + m_pos, toCopy);
		m_pos += toCopy;
		*pcbRead = toCopy;
		return S_OK;
	}

	STDMETHOD(Seek)(LARGE_INTEGER, DWORD, ULARGE_INTEGER*) { return E_NOTIMPL; }
	STDMETHOD(LockRequest)(DWORD) { return S_OK; }
	STDMETHOD(UnlockRequest)() { return S_OK; }

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
	STDMETHOD(CreateInstance)(IUnknown*, REFIID riid, void** ppv)
	{
		VFSHandler* obj = new VFSHandler();
		return obj->QueryInterface(riid, ppv);
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
        static const GUID CLSID_VFSProto =
        { 0x1112f5db, 0x7847, 0x4da3, { 0xbb, 0x61, 0xb3, 0x57, 0xa, 0xfb, 0x27, 0x62 } };

        session->RegisterNameSpace(new VFSHandlerFactory(), CLSID_VFSProto, L"vfs", 0, nullptr, 0);
    }
}

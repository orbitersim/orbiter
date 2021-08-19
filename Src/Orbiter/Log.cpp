// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#define __LOG_CPP

#include <string.h>
#include <fstream>
#include <dx7\ddraw.h>
#include <dx7\dplay.h>
#include <dinput.h>
#include "Log.h"
#include "Orbiter.h"

using namespace std;

extern char DBG_MSG[256];
extern TimeData td;

char logname[256] = "Orbiter.log";
char logs[256] = "";
bool finelog = false;

LogOutFunc logOut = 0;

void InitLog (char *logfile, bool append)
{
	strcpy (logname, logfile);
	ofstream ofs (logname, append ? ios::app : ios::out);
	ofs << "**** " << logname << endl;
}

void SetLogOutFunc(LogOutFunc func)
{
	logOut = func;
}

void SetLogVerbosity (bool verbose)
{
	finelog = verbose;
}

void LogOut (const char *msg, ...)
{
	va_list ap;
	va_start (ap, msg);
	LogOutVA(msg, ap);
	va_end (ap);
}

void LogOutVA(const char *format, va_list ap)
{
	extern TimeData td;
	FILE *f = fopen(logname, "a+t");
	fprintf(f, "%010.3f: ", td.SysT0);
	vfprintf(f, format, ap);
	fputc('\n', f);
	fclose(f);
	if (logOut) {
		vsnprintf(logs, 255, format, ap);
		(*logOut)(logs);
	}
}

void LogOutFine (const char *msg, ...)
{
	if (finelog) {
		extern TimeData td;
		va_list ap;
		va_start (ap, msg);
		FILE *f = fopen (logname, "a+t");
		fprintf (f, "%010.3f: ", td.SysT0);
		vfprintf (f, msg, ap);
		fputc ('\n', f);
		fclose (f);
		if (logOut) {
			vsnprintf (logs, 255, msg, ap);
			(*logOut)(logs);
		}
		va_end (ap);
	}
}

void LogOut ()
{
	LogOut (logs);
}

void LogOut_Error (const char *func, const char *file, int line, const char *msg, ...)
{
	va_list ap;
	va_start (ap, msg);
	LogOut_ErrorVA(func, file, line, msg, ap);
	va_end(ap);
}

void LogOut_Error_Start()
{
	LogOut("============================ ERROR: ===========================");
}

void LogOut_Error_End()
{
	LogOut("===============================================================");
}

void LogOut_Location(const char* func, const char* file, int line)
{
	LogOut("[%s | %s | %d]", func, file, line);

}

void LogOut_ErrorVA(const char *func, const char *file, int line, const char *msg, va_list ap)
{
	LogOut_Error_Start();
	LogOutVA(msg, ap);
	LogOut_Location(func, file, line);
	LogOut_Error_End();
}

void LogOut_LastError (const char *func, const char *file, int line)
{
	DWORD err = GetLastError();
	LPTSTR errString = NULL;
	FormatMessage (FORMAT_MESSAGE_FROM_SYSTEM|FORMAT_MESSAGE_ALLOCATE_BUFFER|FORMAT_MESSAGE_IGNORE_INSERTS,NULL,err,MAKELANGID(LANG_NEUTRAL,SUBLANG_DEFAULT),(LPTSTR)&errString,0,NULL);
	LogOut_Error (func, file, line, errString);
	if (errString) LocalFree(errString);
}

void LogOut_DDErr (HRESULT hr, const char *func, const char *file, int line) {
	if (hr == DD_OK) return;
	static char errmsg[256] = ">>> ERROR: DDraw error ";
	static char *err = errmsg+23;
	switch (hr) {
	case DDERR_ALREADYINITIALIZED:			strcpy (err, "DDERR_ALREADYINITIALIZED"); break;
	case DDERR_CANNOTATTACHSURFACE:			strcpy (err, "DDERR_CANNOTATTACHSURFACE"); break;
	case DDERR_CANNOTDETACHSURFACE:			strcpy (err, "DDERR_CANNOTDETACHSURFACE"); break;
	case DDERR_CURRENTLYNOTAVAIL:			strcpy (err, "DDERR_CURRENTLYNOTAVAIL"); break;
	case DDERR_EXCEPTION:					strcpy (err, "DDERR_EXCEPTION"); break;
	case DDERR_GENERIC:						strcpy (err, "DDERR_GENERIC"); break;
	case DDERR_HEIGHTALIGN:					strcpy (err, "DDERR_HEIGHTALIGN"); break;
	case DDERR_INCOMPATIBLEPRIMARY:			strcpy (err, "DDERR_INCOMPATIBLEPRIMARY"); break;
	case DDERR_INVALIDCAPS:					strcpy (err, "DDERR_INVALIDCAPS"); break;
	case DDERR_INVALIDCLIPLIST:				strcpy (err, "DDERR_INVALIDCLIPLIST"); break;
	case DDERR_INVALIDMODE:					strcpy (err, "DDERR_INVALIDMODE"); break;
	case DDERR_INVALIDOBJECT:				strcpy (err, "DDERR_INVALIDOBJECT"); break;
	case DDERR_INVALIDPARAMS:				strcpy (err, "DDERR_INVALIDPARAMS"); break;
	case DDERR_INVALIDPIXELFORMAT:			strcpy (err, "DDERR_INVALIDPIXELFORMAT"); break;
	case DDERR_INVALIDRECT:					strcpy (err, "DDERR_INVALIDRECT"); break;
	case DDERR_LOCKEDSURFACES:				strcpy (err, "DDERR_LOCKEDSURFACES"); break;
	case DDERR_NO3D:						strcpy (err, "DDERR_NO3D"); break;
	case DDERR_NOALPHAHW:					strcpy (err, "DDERR_NOALPHAHW"); break;
	case DDERR_NOSTEREOHARDWARE:			strcpy (err, "DDERR_NOSTEREOHARDWARE"); break;
	case DDERR_NOSURFACELEFT:				strcpy (err, "DDERR_NOSURFACELEFT"); break;
	case DDERR_NOCLIPLIST:					strcpy (err, "DDERR_NOCLIPLIST"); break;
	case DDERR_NOCOLORCONVHW:				strcpy (err, "DDERR_NOCOLORCONVHW"); break;
	case DDERR_NOCOOPERATIVELEVELSET:		strcpy (err, "DDERR_NOCOOPERATIVELEVELSET"); break;
	case DDERR_NOCOLORKEY:					strcpy (err, "DDERR_NOCOLORKEY"); break;
	case DDERR_NOCOLORKEYHW:				strcpy (err, "DDERR_NOCOLORKEYHW"); break;
	case DDERR_NODIRECTDRAWSUPPORT:			strcpy (err, "DDERR_NODIRECTDRAWSUPPORT"); break;
	case DDERR_NOEXCLUSIVEMODE:				strcpy (err, "DDERR_NOEXCLUSIVEMODE"); break;
	case DDERR_NOFLIPHW:					strcpy (err, "DDERR_NOFLIPHW"); break;
	case DDERR_NOGDI:						strcpy (err, "DDERR_NOGDI"); break;
	case DDERR_NOMIRRORHW:					strcpy (err, "DDERR_NOMIRRORHW"); break;
	case DDERR_NOTFOUND:					strcpy (err, "DDERR_NOTFOUND"); break;
	case DDERR_NOOVERLAYHW:					strcpy (err, "DDERR_NOOVERLAYHW"); break;
	case DDERR_OVERLAPPINGRECTS:			strcpy (err, "DDERR_OVERLAPPINGRECTS"); break;
	case DDERR_NORASTEROPHW:				strcpy (err, "DDERR_NORASTEROPHW"); break;
	case DDERR_NOROTATIONHW:				strcpy (err, "DDERR_NOROTATIONHW"); break;
	case DDERR_NOSTRETCHHW:					strcpy (err, "DDERR_NOSTRETCHHW"); break;
	case DDERR_NOT4BITCOLOR:				strcpy (err, "DDERR_NOT4BITCOLOR"); break;
	case DDERR_NOT4BITCOLORINDEX:			strcpy (err, "DDERR_NOT4BITCOLORINDEX"); break;
	case DDERR_NOT8BITCOLOR:				strcpy (err, "DDERR_NOT8BITCOLOR"); break;
	case DDERR_NOTEXTUREHW:					strcpy (err, "DDERR_NOTEXTUREHW"); break;
	case DDERR_NOVSYNCHW:					strcpy (err, "DDERR_NOVSYNCHW"); break;
	case DDERR_NOZBUFFERHW:					strcpy (err, "DDERR_NOZBUFFERHW"); break;
	case DDERR_NOZOVERLAYHW:				strcpy (err, "DDERR_NOZOVERLAYHW"); break;
	case DDERR_OUTOFCAPS:					strcpy (err, "DDERR_OUTOFCAPS"); break;
	case DDERR_OUTOFMEMORY:					strcpy (err, "DDERR_OUTOFMEMORY"); break;
	case DDERR_OUTOFVIDEOMEMORY:			strcpy (err, "DDERR_OUTOFVIDEOMEMORY"); break;
	case DDERR_OVERLAYCANTCLIP:				strcpy (err, "DDERR_OVERLAYCANTCLIP"); break;
	case DDERR_OVERLAYCOLORKEYONLYONEACTIVE:strcpy (err, "DDERR_OVERLAYCOLORKEYONLYONEACTIVE"); break;
	case DDERR_PALETTEBUSY:					strcpy (err, "DDERR_PALETTEBUSY"); break;
	case DDERR_COLORKEYNOTSET:				strcpy (err, "DDERR_COLORKEYNOTSET"); break;
	case DDERR_SURFACEALREADYATTACHED:		strcpy (err, "DDERR_SURFACEALREADYATTACHED"); break;
	case DDERR_SURFACEALREADYDEPENDENT:		strcpy (err, "DDERR_SURFACEALREADYDEPENDENT"); break;
	case DDERR_SURFACEBUSY:					strcpy (err, "DDERR_SURFACEBUSY"); break;
	case DDERR_CANTLOCKSURFACE:				strcpy (err, "DDERR_CANTLOCKSURFACE"); break;
	case DDERR_SURFACEISOBSCURED:			strcpy (err, "DDERR_SURFACEISOBSCURED"); break;
	case DDERR_SURFACELOST:					strcpy (err, "DDERR_SURFACELOST"); break;
	case DDERR_SURFACENOTATTACHED:			strcpy (err, "DDERR_SURFACENOTATTACHED"); break;
	case DDERR_TOOBIGHEIGHT:				strcpy (err, "DDERR_TOOBIGHEIGHT"); break;
	case DDERR_TOOBIGSIZE:					strcpy (err, "DDERR_TOOBIGSIZE"); break;
	case DDERR_TOOBIGWIDTH:					strcpy (err, "DDERR_TOOBIGWIDTH"); break;
	case DDERR_UNSUPPORTED:					strcpy (err, "DDERR_UNSUPPORTED"); break;
	case DDERR_UNSUPPORTEDFORMAT:			strcpy (err, "DDERR_UNSUPPORTEDFORMAT"); break;
	case DDERR_UNSUPPORTEDMASK:				strcpy (err, "DDERR_UNSUPPORTEDMASK"); break;
	case DDERR_INVALIDSTREAM:				strcpy (err, "DDERR_INVALIDSTREAM"); break;
	case DDERR_VERTICALBLANKINPROGRESS:		strcpy (err, "DDERR_VERTICALBLANKINPROGRESS"); break;
	case DDERR_WASSTILLDRAWING:				strcpy (err, "DDERR_WASSTILLDRAWING"); break;
	case DDERR_DDSCAPSCOMPLEXREQUIRED:		strcpy (err, "DDERR_DDSCAPSCOMPLEXREQUIRED"); break;
	case DDERR_XALIGN:						strcpy (err, "DDERR_XALIGN"); break;
	case DDERR_INVALIDDIRECTDRAWGUID:		strcpy (err, "DDERR_INVALIDDIRECTDRAWGUID"); break;
	case DDERR_DIRECTDRAWALREADYCREATED:	strcpy (err, "DDERR_DIRECTDRAWALREADYCREATED"); break;
	case DDERR_NODIRECTDRAWHW:				strcpy (err, "DDERR_NODIRECTDRAWHW"); break;
	case DDERR_PRIMARYSURFACEALREADYEXISTS: strcpy (err, "DDERR_PRIMARYSURFACEALREADYEXISTS"); break;
	case DDERR_NOEMULATION:					strcpy (err, "DDERR_NOEMULATION"); break;
	case DDERR_REGIONTOOSMALL:				strcpy (err, "DDERR_REGIONTOOSMALL"); break;
	case DDERR_CLIPPERISUSINGHWND:			strcpy (err, "DDERR_CLIPPERISUSINGHWND"); break;
	case DDERR_NOCLIPPERATTACHED:			strcpy (err, "DDERR_NOCLIPPERATTACHED"); break;
	case DDERR_NOHWND:						strcpy (err, "DDERR_NOHWND"); break;
	case DDERR_HWNDSUBCLASSED:				strcpy (err, "DDERR_HWNDSUBCLASSED"); break;
	case DDERR_HWNDALREADYSET:				strcpy (err, "DDERR_HWNDALREADYSET"); break;
	case DDERR_NOPALETTEATTACHED:			strcpy (err, "DDERR_NOPALETTEATTACHED"); break;
	case DDERR_NOPALETTEHW:					strcpy (err, "DDERR_NOPALETTEHW"); break;
	case DDERR_BLTFASTCANTCLIP:				strcpy (err, "DDERR_BLTFASTCANTCLIP"); break;
	case DDERR_NOBLTHW:						strcpy (err, "DDERR_NOBLTHW"); break;
	case DDERR_NODDROPSHW:					strcpy (err, "DDERR_NODDROPSHW"); break;
	case DDERR_OVERLAYNOTVISIBLE:			strcpy (err, "DDERR_OVERLAYNOTVISIBLE"); break;
	case DDERR_NOOVERLAYDEST:				strcpy (err, "DDERR_NOOVERLAYDEST"); break;
	case DDERR_INVALIDPOSITION:				strcpy (err, "DDERR_INVALIDPOSITION"); break;
	case DDERR_NOTAOVERLAYSURFACE:			strcpy (err, "DDERR_NOTAOVERLAYSURFACE"); break;
	case DDERR_EXCLUSIVEMODEALREADYSET:		strcpy (err, "DDERR_EXCLUSIVEMODEALREADYSET"); break;
	case DDERR_NOTFLIPPABLE:				strcpy (err, "DDERR_NOTFLIPPABLE"); break;
	case DDERR_CANTDUPLICATE:				strcpy (err, "DDERR_CANTDUPLICATE"); break;
	case DDERR_NOTLOCKED:					strcpy (err, "DDERR_NOTLOCKED"); break;
	case DDERR_CANTCREATEDC:				strcpy (err, "DDERR_CANTCREATEDC"); break;
	case DDERR_NODC:						strcpy (err, "DDERR_NODC"); break;
	case DDERR_WRONGMODE:					strcpy (err, "DDERR_WRONGMODE"); break;
	case DDERR_IMPLICITLYCREATED:			strcpy (err, "DDERR_IMPLICITLYCREATED"); break;
	case DDERR_NOTPALETTIZED:				strcpy (err, "DDERR_NOTPALETTIZED"); break;
	case DDERR_UNSUPPORTEDMODE :			strcpy (err, "DDERR_UNSUPPORTEDMODE"); break;
	case DDERR_NOMIPMAPHW:					strcpy (err, "DDERR_NOMIPMAPHW"); break;
	case DDERR_INVALIDSURFACETYPE:			strcpy (err, "DDERR_INVALIDSURFACETYPE"); break;
	case DDERR_NOOPTIMIZEHW:				strcpy (err, "DDERR_NOOPTIMIZEHW"); break;
	case DDERR_NOTLOADED:					strcpy (err, "DDERR_NOTLOADED"); break;
	case DDERR_NOFOCUSWINDOW:				strcpy (err, "DDERR_NOFOCUSWINDOW"); break;
	case DDERR_NOTONMIPMAPSUBLEVEL:			strcpy (err, "DDERR_NOTONMIPMAPSUBLEVEL"); break;
	case DDERR_DCALREADYCREATED:			strcpy (err, "DDERR_DCALREADYCREATED"); break;
	case DDERR_NONONLOCALVIDMEM:			strcpy (err, "DDERR_NONONLOCALVIDMEM"); break;
	case DDERR_CANTPAGELOCK:				strcpy (err, "DDERR_CANTPAGELOCK"); break;
	case DDERR_CANTPAGEUNLOCK:				strcpy (err, "DDERR_CANTPAGEUNLOCK"); break;
	case DDERR_NOTPAGELOCKED:				strcpy (err, "DDERR_NOTPAGELOCKED"); break;
	case DDERR_MOREDATA:					strcpy (err, "DDERR_MOREDATA"); break;
	case DDERR_EXPIRED:						strcpy (err, "DDERR_EXPIRED"); break;
	case DDERR_TESTFINISHED:				strcpy (err, "DDERR_TESTFINISHED"); break;
	case DDERR_NEWMODE:						strcpy (err, "DDERR_NEWMODE"); break;
	case DDERR_D3DNOTINITIALIZED:			strcpy (err, "DDERR_D3DNOTINITIALIZED"); break;
	case DDERR_VIDEONOTACTIVE:				strcpy (err, "DDERR_VIDEONOTACTIVE"); break;
	case DDERR_NOMONITORINFORMATION:		strcpy (err, "DDERR_NOMONITORINFORMATION"); break;
	case DDERR_NODRIVERSUPPORT:				strcpy (err, "DDERR_NODRIVERSUPPORT"); break;
	case DDERR_DEVICEDOESNTOWNSURFACE:		strcpy (err, "DDERR_DEVICEDOESNTOWNSURFACE"); break;
	case DDERR_NOTINITIALIZED:				strcpy (err, "DDERR_NOTINITIALIZED"); break;
	default:								sprintf (err, "DDERR CODE %ld", hr); break;
	}
	LogOut ("---------------------------------------------------------------");
	LogOut (errmsg);
	sprintf (logs, ">>> [%s | %s | %d]", func, file, line);
	LogOut();
	LogOut ("---------------------------------------------------------------");
}

void LogOut_DIErr (HRESULT hr, const char *func, const char *file, int line) {
	static char errmsg[256] = ">>> ERROR: DInput error ";
	static char *err = errmsg+24;
	switch (hr) {
	case DIERR_INPUTLOST:                   strcpy (err, "DIERR_INPUTLOST"); break;
	case DIERR_INVALIDPARAM:				strcpy (err, "DIERR_INVALIDPARAM"); break;
	case DIERR_NOTACQUIRED:                 strcpy (err, "DIERR_NOTACQUIRED"); break;
	case DIERR_NOTINITIALIZED:				strcpy (err, "DIERR_NOTINITIALIZED"); break;
	case DIERR_OBJECTNOTFOUND:				strcpy (err, "DIERR_OBJECTNOTFOUND"); break;
	case DIERR_REPORTFULL:                  strcpy (err, "DIERR_REPORTFULL"); break;
	case DIERR_UNPLUGGED:					strcpy (err, "DIERR_UNPLUGGED"); break;
	case DIERR_UNSUPPORTED:					strcpy (err, "DIERR_UNSUPPORTED"); break;
	case E_HANDLE:                          strcpy (err, "E_HANDLE"); break;
	case E_PENDING:                         strcpy (err, "E_PENDING"); break;
	default:								sprintf (err, "DIERR CODE %ld", hr); break;
	}
	LogOut ("---------------------------------------------------------------");
	LogOut (errmsg);
	sprintf (logs, ">>> [%s | %s | %d]", func, file, line);
	LogOut();
	LogOut ("---------------------------------------------------------------");
}

void LogOut_DPErr (HRESULT hr, const char *func, const char *file, int line) {
	static char errmsg[256] = ">>> ERROR: DPlay error ";
	static char *err = errmsg+23;
	switch (hr) {
	case DPERR_ACCESSDENIED:				strcpy (err, "DPERR_ACCESSDENIED"); break;
	case DPERR_AUTHENTICATIONFAILED:		strcpy (err, "DPERR_AUTHENTICATIONFAILED"); break;
	case DPERR_BUFFERTOOSMALL:				strcpy (err, "DPERR_BUFFERTOOSMALL"); break;
	case DPERR_BUSY:						strcpy (err, "DPERR_BUSY"); break;
	case DPERR_CANNOTCREATESERVER:			strcpy (err, "DPERR_CANNOTCREATESERVER"); break;
	case DPERR_CANTCREATEPLAYER:			strcpy (err, "DPERR_CANTCREATEPLAYER"); break;
	case DPERR_CANTLOADCAPI:				strcpy (err, "DPERR_CANTLOADCAPI"); break;
	case DPERR_CANTLOADSECURITYPACKAGE:		strcpy (err, "DPERR_CANTLOADSECURITYPACKAGE"); break;
	case DPERR_CANTLOADSSPI:				strcpy (err, "DPERR_CANTLOADSSPI"); break;
	case DPERR_CONNECTING:					strcpy (err, "DPERR_CONNECTING"); break;
	case DPERR_CONNECTIONLOST:				strcpy (err, "DPERR_CONNECTIONLOST"); break;
	case DPERR_ENCRYPTIONFAILED:			strcpy (err, "DPERR_ENCRYPTIONFAILED"); break;
	case DPERR_ENCRYPTIONNOTSUPPORTED:		strcpy (err, "DPERR_ENCRYPTIONNOTSUPPORTED"); break;
	case DPERR_INVALIDFLAGS:				strcpy (err, "DPERR_INVALIDFLAGS"); break;
	case DPERR_INVALIDOBJECT:				strcpy (err, "DPERR_INVALIDOBJECT"); break;
	case DPERR_INVALIDPARAMS:				strcpy (err, "DPERR_INVALIDPARAMS"); break;
	case DPERR_INVALIDPASSWORD:				strcpy (err, "DPERR_INVALIDPASSWORD"); break;
	case DPERR_INVALIDPLAYER:				strcpy (err, "DPERR_INVALIDPLAYER"); break;
	case DPERR_INVALIDPRIORITY:				strcpy (err, "DPERR_INVALIDPRIORITY"); break;
	case DPERR_LOGONDENIED:					strcpy (err, "DPERR_LOGONDENIED"); break;
	case DPERR_NOCONNECTION:				strcpy (err, "DPERR_NOCONNECTION"); break;
	case DPERR_NONEWPLAYERS:				strcpy (err, "DPERR_NONEWPLAYERS"); break;
	case DPERR_NOSESSIONS:					strcpy (err, "DPERR_NOSESSIONS"); break;
	case DPERR_NOTLOGGEDIN:					strcpy (err, "DPERR_NOTLOGGEDIN"); break;
	case DPERR_SENDTOOBIG:					strcpy (err, "DPERR_SENDTOOBIG"); break;
	case DPERR_SIGNFAILED:					strcpy (err, "DPERR_SIGNFAILED"); break;
	case DPERR_TIMEOUT:						strcpy (err, "DPERR_TIMEOUT"); break;
	case DPERR_UNINITIALIZED:				strcpy (err, "DPERR_UNINITIALIZED"); break;
	case DPERR_UNSUPPORTED:					strcpy (err, "DPERR_UNSUPPORTED"); break;
	case DPERR_USERCANCEL:					strcpy (err, "DPERR_USERCANCEL"); break;
	default:								sprintf (err, "DPERR CODE %ld", hr); break;
	}
	LogOut ("---------------------------------------------------------------");
	LogOut (errmsg);
	sprintf (logs, ">>> [%s | %s | %d]", func, file, line);
	LogOut();
	LogOut ("---------------------------------------------------------------");
}

void LogOut_Obsolete (char *func, char *msg)
{
	LogOut ("---------------------------------------------------------------");
	strcpy (logs, ">>> WARNING: Obsolete API function used: ");
	strcat (logs, func);
	LogOut ();
	if (msg) LogOut (msg);
	else {
		LogOut ("At least one active module is accessing an obsolete interface function.");
		LogOut ("Addons which rely on obsolete functions may not be compatible with");
		LogOut ("future versions of Orbiter.");
	}
	LogOut ("---------------------------------------------------------------");
}

void LogOut_Warning (const char *func, const char *file, int line, const char *msg, ...)
{
	va_list ap;
	FILE *f = fopen (logname, "a+t");
	fputs ("--------------------------- WARNING: --------------------------\n>>> ", f);
	va_start (ap,msg);
	vfprintf (f, msg, ap);
	va_end(ap);
	fprintf (f, "\n>>> [%s | %s | %d]\n", func, file, line);
	fputs ("---------------------------------------------------------------\n", f);
	fclose (f);
}

void tracenew (char *fname, int line)
{
#define TESTALLOC 2
#if TESTALLOC == 1
	ofstream ofs("tracenew.txt", ios::app);
	sprintf (DBG_MSG, "T=%f, %s: %d", SimT, fname, line);
	ofs << DBG_MSG << endl;
	ofs.close();
#elif TESTALLOC == 2
	sprintf (DBG_MSG, "T=%f, %s: %d", td.SimT0, fname, line);
#else
	MessageBeep (-1);
#endif
}


// =======================================================================
// Profiler methods
// =======================================================================

static double prof_sum = 0.0;
static double prof_scale = 0.0;
static DWORD prof_count = 0;
static LARGE_INTEGER prof_t0;

void StartProf ()
{
	if (!prof_scale) {
		LARGE_INTEGER freq;
		QueryPerformanceFrequency (&freq);
		double f = (double)freq.LowPart + (double)freq.HighPart * 4294967296.0;
		prof_scale = 1e6/(double)f;
	}
	QueryPerformanceCounter (&prof_t0);
}

double EndProf (DWORD *count)
{
	LARGE_INTEGER t1;
	QueryPerformanceCounter (&t1);
	double f0 = (double)prof_t0.LowPart + (double)prof_t0.HighPart * 4294967296.0;
	double f1 = (double)t1.LowPart      + (double)t1.HighPart * 4294967296.0;
	double dt = f1-f0;
	if (dt > 0.0) {
		prof_sum += dt;
		prof_count++;
	}
	if (count) *count = prof_count;
	return prof_sum*prof_scale/(double)prof_count;
}


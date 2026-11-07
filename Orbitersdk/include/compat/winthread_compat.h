#pragma once

#define WAIT_OBJECT_0 0x00000000L
#define WAIT_ABANDONED 0x00000080L
#define WAIT_TIMEOUT 0x00000102L
#define WAIT_FAILED 0xFFFFFFFF
#define STILL_ACTIVE 259

typedef unsigned (*OCOMPAT_THREADPROC)(void *);

OCOMPAT_API HANDLE CreateMutexA(void *sa, WINBOOL initialOwner, const char *name);
#define CreateMutex CreateMutexA
OCOMPAT_API WINBOOL ReleaseMutex(HANDLE h);
OCOMPAT_API HANDLE CreateEventA(void *sa, WINBOOL manualReset, WINBOOL initialState, const char *name);
#define CreateEvent CreateEventA
OCOMPAT_API WINBOOL SetEvent(HANDLE h);
OCOMPAT_API WINBOOL ResetEvent(HANDLE h);
OCOMPAT_API DWORD WaitForSingleObject(HANDLE h, DWORD timeout);
OCOMPAT_API WINBOOL CloseHandle(HANDLE h);
OCOMPAT_API uintptr_t _beginthreadex(void *sa, unsigned stack, OCOMPAT_THREADPROC proc, void *arg, unsigned flags, unsigned *id);
OCOMPAT_API void _endthreadex(unsigned code);
OCOMPAT_API HANDLE CreateThread(void *sa, SIZE_T stack, LPTHREAD_START_ROUTINE proc, void *arg, DWORD flags, DWORD *id);
OCOMPAT_API WINBOOL TerminateThread(HANDLE h, DWORD code);
OCOMPAT_API WINBOOL GetExitCodeThread(HANDLE h, DWORD *code);
OCOMPAT_API DWORD GetCurrentThreadId(void);

#include <pthread.h>
typedef struct _CRITICAL_SECTION { pthread_mutex_t m; } CRITICAL_SECTION, *LPCRITICAL_SECTION;
inline void InitializeCriticalSection(CRITICAL_SECTION *cs)
{
	pthread_mutexattr_t a;
	pthread_mutexattr_init(&a);
	pthread_mutexattr_settype(&a, PTHREAD_MUTEX_RECURSIVE);
	pthread_mutex_init(&cs->m, &a);
	pthread_mutexattr_destroy(&a);
}
inline WINBOOL InitializeCriticalSectionAndSpinCount(CRITICAL_SECTION *cs, DWORD) { InitializeCriticalSection(cs); return TRUE; }
inline void EnterCriticalSection(CRITICAL_SECTION *cs) { pthread_mutex_lock(&cs->m); }
inline WINBOOL TryEnterCriticalSection(CRITICAL_SECTION *cs) { return pthread_mutex_trylock(&cs->m) == 0; }
inline void LeaveCriticalSection(CRITICAL_SECTION *cs) { pthread_mutex_unlock(&cs->m); }
inline void DeleteCriticalSection(CRITICAL_SECTION *cs) { pthread_mutex_destroy(&cs->m); }

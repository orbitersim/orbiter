#include <windows.h>
#include <pthread.h>
#include <mutex>
#include <condition_variable>
#include <chrono>
#include <thread>

namespace {

struct Handle {
	virtual ~Handle() {}
	virtual DWORD Wait(DWORD timeout) = 0;
};

template<typename Pred>
bool WaitFor(std::unique_lock<std::mutex> &lk, std::condition_variable &cv, DWORD timeout, Pred pred)
{
	if (timeout == INFINITE) {
		cv.wait(lk, pred);
		return true;
	}
	return cv.wait_for(lk, std::chrono::milliseconds(timeout), pred);
}

struct Mutex : Handle {
	std::mutex m;
	std::condition_variable cv;
	std::thread::id owner;
	int count = 0;

	DWORD Wait(DWORD timeout) override
	{
		std::unique_lock<std::mutex> lk(m);
		auto self = std::this_thread::get_id();
		if (count && owner == self) {
			count++;
			return WAIT_OBJECT_0;
		}
		if (!WaitFor(lk, cv, timeout, [&] { return count == 0; })) return WAIT_TIMEOUT;
		owner = self;
		count = 1;
		return WAIT_OBJECT_0;
	}

	bool Release()
	{
		std::lock_guard<std::mutex> lk(m);
		if (!count || owner != std::this_thread::get_id()) return false;
		if (--count == 0) {
			owner = std::thread::id();
			cv.notify_one();
		}
		return true;
	}
};

struct Event : Handle {
	std::mutex m;
	std::condition_variable cv;
	bool manual = false;
	bool signalled = false;

	DWORD Wait(DWORD timeout) override
	{
		std::unique_lock<std::mutex> lk(m);
		if (!WaitFor(lk, cv, timeout, [&] { return signalled; })) return WAIT_TIMEOUT;
		if (!manual) signalled = false;
		return WAIT_OBJECT_0;
	}
};

struct Thread : Handle {
	pthread_t th = nullptr;
	std::mutex m;
	std::condition_variable cv;
	bool done = false;
	bool detached = false;
	bool released = false;
	DWORD exitCode = STILL_ACTIVE;
	OCOMPAT_THREADPROC proc = nullptr;
	LPTHREAD_START_ROUTINE proc2 = nullptr;
	void *arg = nullptr;

	DWORD Wait(DWORD timeout) override
	{
		std::unique_lock<std::mutex> lk(m);
		if (!WaitFor(lk, cv, timeout, [&] { return done; })) return WAIT_TIMEOUT;
		return WAIT_OBJECT_0;
	}

	DWORD pendingCode = 0;

	void Finish()
	{
		bool del;
		{
			std::lock_guard<std::mutex> lk(m);
			if (done) return;
			done = true;
			exitCode = pendingCode;
			cv.notify_all();
			del = released;
		}
		if (del) delete this;
	}
};

thread_local Thread *t_current = nullptr;

void ThreadExitCleanup(void *p)
{
	t_current = nullptr;
	((Thread *)p)->Finish();
}

void *ThreadMain(void *p)
{
	Thread *t = (Thread *)p;
	t_current = t;
	pthread_cleanup_push(ThreadExitCleanup, t);
	t->pendingCode = t->proc ? t->proc(t->arg) : t->proc2(t->arg);
	pthread_cleanup_pop(1);
	return nullptr;
}

Thread *StartThread(Thread *t, size_t stack)
{
	pthread_attr_t attr;
	pthread_attr_init(&attr);
	if (stack < 8 * 1024 * 1024) stack = 8 * 1024 * 1024;
	pthread_attr_setstacksize(&attr, stack);
	pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
	int r = pthread_create(&t->th, &attr, ThreadMain, t);
	pthread_attr_destroy(&attr);
	if (r != 0) {
		delete t;
		return nullptr;
	}
	return t;
}

}

extern "C" {

HANDLE CreateMutexA(void *, WINBOOL initialOwner, const char *)
{
	Mutex *m = new Mutex;
	if (initialOwner) {
		m->owner = std::this_thread::get_id();
		m->count = 1;
	}
	return (HANDLE)static_cast<Handle *>(m);
}

WINBOOL ReleaseMutex(HANDLE h)
{
	Mutex *m = dynamic_cast<Mutex *>((Handle *)h);
	return m && m->Release();
}

HANDLE CreateEventA(void *, WINBOOL manualReset, WINBOOL initialState, const char *)
{
	Event *e = new Event;
	e->manual = manualReset;
	e->signalled = initialState;
	return (HANDLE)static_cast<Handle *>(e);
}

WINBOOL SetEvent(HANDLE h)
{
	Event *e = dynamic_cast<Event *>((Handle *)h);
	if (!e) return FALSE;
	std::lock_guard<std::mutex> lk(e->m);
	e->signalled = true;
	if (e->manual) e->cv.notify_all();
	else e->cv.notify_one();
	return TRUE;
}

WINBOOL ResetEvent(HANDLE h)
{
	Event *e = dynamic_cast<Event *>((Handle *)h);
	if (!e) return FALSE;
	std::lock_guard<std::mutex> lk(e->m);
	e->signalled = false;
	return TRUE;
}

DWORD WaitForSingleObject(HANDLE h, DWORD timeout)
{
	if (!h || h == INVALID_HANDLE_VALUE) return WAIT_FAILED;
	return ((Handle *)h)->Wait(timeout);
}

WINBOOL CloseHandle(HANDLE h)
{
	if (!h || h == INVALID_HANDLE_VALUE) return FALSE;
	Handle *base = (Handle *)h;
	if (Thread *t = dynamic_cast<Thread *>(base)) {
		std::unique_lock<std::mutex> lk(t->m);
		if (!t->done) {
			t->released = true;
			return TRUE;
		}
		lk.unlock();
		delete t;
		return TRUE;
	}
	delete base;
	return TRUE;
}

uintptr_t _beginthreadex(void *, unsigned stack, OCOMPAT_THREADPROC proc, void *arg, unsigned, unsigned *id)
{
	Thread *t = new Thread;
	t->proc = proc;
	t->arg = arg;
	t = StartThread(t, stack);
	if (t && id) *id = (unsigned)(uintptr_t)t;
	return (uintptr_t)static_cast<Handle *>(t);
}

HANDLE CreateThread(void *, SIZE_T stack, LPTHREAD_START_ROUTINE proc, void *arg, DWORD, DWORD *id)
{
	Thread *t = new Thread;
	t->proc2 = proc;
	t->arg = arg;
	t = StartThread(t, stack);
	if (t && id) *id = (DWORD)(uintptr_t)t;
	return t ? (HANDLE)static_cast<Handle *>(t) : nullptr;
}

void _endthreadex(unsigned code)
{
	if (t_current) t_current->pendingCode = code;
	pthread_exit(nullptr);
}

WINBOOL TerminateThread(HANDLE h, DWORD code)
{
	Thread *t = dynamic_cast<Thread *>((Handle *)h);
	if (!t) return FALSE;
	{
		std::lock_guard<std::mutex> lk(t->m);
		if (t->done) return TRUE;
	}
	t->pendingCode = code;
	pthread_cancel(t->th);
	return t->Wait(1000) == WAIT_OBJECT_0;
}

WINBOOL GetExitCodeThread(HANDLE h, DWORD *code)
{
	Thread *t = dynamic_cast<Thread *>((Handle *)h);
	if (!t || !code) return FALSE;
	std::lock_guard<std::mutex> lk(t->m);
	*code = t->exitCode;
	return TRUE;
}

DWORD GetCurrentThreadId(void)
{
	uint64_t tid;
	pthread_threadid_np(nullptr, &tid);
	return (DWORD)tid;
}

}

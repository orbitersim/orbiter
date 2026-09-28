// not upstream: a console tool started from the file manager reopens in a terminal window, as Windows gives it a console

#pragma once

#include <sys/stat.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <vector>

// stdin and stdout of a desktop launch: /dev/null or closed in, the journal socket or /dev/null out
inline bool ToolFdNull (int fd) { struct stat s; return fstat (fd, &s) || (S_ISCHR (s.st_mode) && !isatty (fd)); }
inline bool ToolFdLog (int fd) { struct stat s; return fstat (fd, &s) || S_ISSOCK (s.st_mode) || (S_ISCHR (s.st_mode) && !isatty (fd)); }
inline bool ToolEnv (const char *name) { const char *v = getenv (name); return v && *v; }

inline void OpenToolTerminal (int argc, char *argv[])
{
	if (const char *dir = getenv ("ORBITER_TOOL_DIR")) { // the relaunch: Explorer starts a program in its own folder
		if (chdir (dir)) {}
		unsetenv ("ORBITER_TOOL_DIR");
		return;
	}
	if (!ToolFdNull (STDIN_FILENO) || !ToolFdLog (STDOUT_FILENO)) return; // a terminal, a pipe or a file: run here
	if (!ToolEnv ("WAYLAND_DISPLAY") && !ToolEnv ("DISPLAY")) return;
	char self[4096];
	ssize_t n = readlink ("/proc/self/exe", self, sizeof(self) - 1);
	if (n <= 0) return;
	self[n] = '\0';
	char dir[4096];
	strcpy (dir, self);
	if (char *slash = strrchr (dir, '/')) *slash = '\0';
	setenv ("ORBITER_TOOL_DIR", dir, 1);
	const char *term[][2] = {
		{getenv ("TERMINAL"), "-e"}, {"x-terminal-emulator", "-e"}, {"konsole", "-e"},
		{"gnome-terminal", "--"}, {"xfce4-terminal", "-x"}, {"xterm", "-e"}
	};
	for (auto &t : term) {
		if (!t[0] || !*t[0]) continue;
		std::vector<char*> av = { (char*)t[0], (char*)t[1], self };
		for (int i = 1; i < argc; i++) av.push_back (argv[i]);
		av.push_back (0);
		execvp (t[0], av.data ()); // returns only when that terminal is not installed
	}
	unsetenv ("ORBITER_TOOL_DIR"); // no terminal found: run without one
}

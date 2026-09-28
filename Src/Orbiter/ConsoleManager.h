#pragma once
#ifdef __linux__

class QWindow;
#endif // __linux__

class ConsoleManager {
public:
    static bool IsConsoleExclusive(void);
    static void ShowConsole(bool show);
#ifdef __linux__
    // Orbiter's own console window, the one Windows gives a console program; only without a terminal
    static QWindow* ConsoleWindow(void);                        // GetConsoleWindow
    static void SetConsoleTitle(const char* title);             // SetConsoleTitle
    static bool WriteConsole(const char* text);                 // WriteConsole, any thread; false without the window
    static void SetConsoleInput(void (*func)(const char* line)); // ReadConsole: gets each typed line
#endif // __linux__
};

#ifndef __HTMLCTRL_H
#define __HTMLCTRL_H

#ifndef __linux__
extern "C" {
	void RegisterHtmlCtrl (HINSTANCE hInstance, BOOL active = true);
	long DisplayHTMLPage(HWND hwnd, LPTSTR webPageName);
	long WINAPI DisplayHTMLStr(HWND hwnd, const char *string);
}
#else // __linux__
#include "OrbiterPlatform.h"

void RegisterHtmlCtrl (void *hInstance, BOOL active = true);
long DisplayHTMLPage(QWidget *hwnd, const char *webPageName);
long DisplayHTMLStr(QWidget *hwnd, const char *string);
#endif // __linux__

#endif // !__HTMLCTRL_H

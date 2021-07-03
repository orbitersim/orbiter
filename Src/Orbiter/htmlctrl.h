#ifndef __HTMLCTRL_H
#define __HTMLCTRL_H

extern "C" {
	void RegisterHtmlCtrl (HINSTANCE hInstance, BOOL active = true);
	long DisplayHTMLPage(HWND hwnd, LPTSTR webPageName);
	long WINAPI DisplayHTMLStr(HWND hwnd, const char *string);
}

#endif // !__HTMLCTRL_H
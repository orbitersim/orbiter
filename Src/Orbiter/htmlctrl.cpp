// HtmlCtrl: the dialog control that shows scenario descriptions as HTML.
// Upstream embedded the Internet Explorer OLE browser object; QTextBrowser renders the pages here.

#include "htmlctrl.h"
#include "ChmHelp.h"
#include "OrbiterResource.h"
#include <QFileInfo>
#include <QTextBrowser>
#include <QUrl>

static bool g_active = true;

// window procedure of the control: embeds the browser object on creation (WM_CREATE)
static QWidget *CreateHtmlCtrl (const RESCONTROL*, QWidget *parent)
{
	if (!g_active) return new QWidget (parent); // WindowProcDummy
	QTextBrowser *tb = new ChmBrowser (parent); // also shows pages inside help files ("its:" URLs)
	tb->setOpenLinks (true);          // internal links navigate inside the control, like the browser object
	tb->setOpenExternalLinks (true);  // web links go to the desktop browser
	tb->setContextMenuPolicy (Qt::NoContextMenu); // the pop-up context menu was disabled
	return tb;
}

// Displays a URL, or HTML file on disk. Returns 0 if success, or non-zero if an error.
long DisplayHTMLPage(QWidget *hwnd, const char *webPageName)
{
	QTextBrowser *tb = qobject_cast<QTextBrowser*> (hwnd);
	if (!tb || !webPageName) return -1;
	QString name = QString::fromUtf8 (webPageName);
	QUrl url (name);
	if (name.contains (".chm::", Qt::CaseInsensitive)) url = ChmUrlFromIts (name); // page inside a help file
	else if (url.scheme().size() <= 1) // plain path (a one-letter scheme would be a Windows drive)
		url = QUrl::fromLocalFile (QFileInfo (name).absoluteFilePath());
	tb->setSource (url);
	return 0;
}

// Displays an HTML string (the BODY contents, no <BODY></BODY> tags required).
// Returns 0 if success, or non-zero if an error.
long DisplayHTMLStr(QWidget *hwnd, const char *string)
{
	QTextBrowser *tb = qobject_cast<QTextBrowser*> (hwnd);
	if (!tb || !string) return -1;
	if (ChmBrowser *cb = dynamic_cast<ChmBrowser*> (tb)) cb->SetPageHtml (QString::fromUtf8 (string));
	else tb->setHtml (QString::fromUtf8 (string));
	return 0;
}

void RegisterHtmlCtrl (void *hInstance, BOOL active)
{
	g_active = (active != FALSE);

	// Register the class of our window to host the browser.
	oapiRegisterResControl (hInstance, "HtmlCtrl", CreateHtmlCtrl);
}

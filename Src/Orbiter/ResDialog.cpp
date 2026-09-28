// not upstream: builds Qt dialogs from the resource tables rc2cpp.py compiles out of the .rc scripts

#include "ResDialog.h"
#include "Util.h"
#include "Log.h"
#include <QAction>
#include <QApplication>
#include <QBuffer>
#include <QButtonGroup>
#include <QCheckBox>
#include <QComboBox>
#include <QDialog>
#include <QFontMetrics>
#include <QFrame>
#include <QGroupBox>
#include <QHeaderView>
#include <QImage>
#include <QImageReader>
#include <QLabel>
#include <QLineEdit>
#include <QListWidget>
#include <QMenu>
#include <QMenuBar>
#include <QPainter>
#include <QPixmap>
#include <QPlainTextEdit>
#include <QProxyStyle>
#include <QProgressBar>
#include <QPushButton>
#include <QRadioButton>
#include <QRegularExpressionValidator>
#include <QResizeEvent>
#include <QScreen>
#include <QScrollBar>
#include <QSlider>
#include <QStringDecoder>
#include <QStyleFactory>
#include <QStyleOption>
#include <QTextBrowser>
#include <QTextEdit>
#include <QToolButton>
#include <QTreeWidget>
#include <QWindow>
#include <algorithm>
#include <cmath>
#include <cstdarg>
#include <cstdio>
#include <cstring>
#include <map>
#include <string>
#include <vector>
#include <strings.h>
#include <dlfcn.h>
#include <elf.h>
#include <fstream>

// Win32 style bits as they come from the .rc
namespace rs {
	constexpr DWORD WS_POPUP = 0x80000000, WS_CHILD = 0x40000000, WS_VISIBLE = 0x10000000, WS_DISABLED = 0x08000000,
		WS_CAPTION = 0x00C00000, WS_BORDER = 0x00800000, WS_VSCROLL = 0x00200000, WS_HSCROLL = 0x00100000,
		WS_THICKFRAME = 0x00040000, WS_GROUP = 0x00020000, WS_MINIMIZEBOX = 0x00020000, WS_MAXIMIZEBOX = 0x00010000;
	constexpr DWORD WS_EX_TOPMOST = 0x8, WS_EX_TOOLWINDOW = 0x80, WS_EX_CLIENTEDGE = 0x200, WS_EX_STATICEDGE = 0x20000,
		WS_EX_APPWINDOW = 0x40000;
	constexpr DWORD DS_CENTER = 0x800;
	constexpr DWORD SS_CENTER = 0x1, SS_RIGHT = 0x2, SS_BLACKRECT = 0x4, SS_GRAYRECT = 0x5, SS_WHITERECT = 0x6,
		SS_BLACKFRAME = 0x7, SS_GRAYFRAME = 0x8, SS_WHITEFRAME = 0x9, SS_SIMPLE = 0xB, SS_LEFTNOWORDWRAP = 0xC,
		SS_ETCHEDHORZ = 0x10, SS_ETCHEDVERT = 0x11, SS_ETCHEDFRAME = 0x12, SS_REALSIZECONTROL = 0x40,
		SS_NOPREFIX = 0x80, SS_CENTERIMAGE = 0x200, SS_REALSIZEIMAGE = 0x800, SS_SUNKEN = 0x1000, SS_ICON = 0x3;
	constexpr DWORD BS_DEFPUSHBUTTON = 0x1, BS_3STATE = 0x5, BS_AUTO3STATE = 0x6, BS_LEFT = 0x100, BS_RIGHT = 0x200,
		BS_PUSHLIKE = 0x1000, BS_FLAT = 0x8000;
	constexpr DWORD ES_CENTER = 0x1, ES_RIGHT = 0x2, ES_MULTILINE = 0x4, ES_PASSWORD = 0x20, ES_AUTOHSCROLL = 0x80,
		ES_READONLY = 0x800, ES_NUMBER = 0x2000;
	constexpr DWORD CBS_SIMPLE = 0x1, CBS_DROPDOWN = 0x2, CBS_SORT = 0x100;
	constexpr DWORD LBS_SORT = 0x2, LBS_MULTIPLESEL = 0x8, LBS_EXTENDEDSEL = 0x800, LBS_NOSEL = 0x4000;
	constexpr DWORD SBS_VERT = 0x1;
	constexpr DWORD TVS_LINESATROOT = 0x4, TVS_CHECKBOXES = 0x100, TVS_FULLROWSELECT = 0x1000, TVS_NOHSCROLL = 0x8000;
	constexpr DWORD TBS_AUTOTICKS = 0x1, TBS_VERT = 0x2, TBS_TOP = 0x4, TBS_BOTH = 0x8, TBS_NOTICKS = 0x10;
	constexpr DWORD UDS_WRAP = 0x1, UDS_SETBUDDYINT = 0x2, UDS_ALIGNRIGHT = 0x4, UDS_ALIGNLEFT = 0x8, UDS_AUTOBUDDY = 0x10,
		UDS_HORZ = 0x40;
	constexpr DWORD PBS_VERTICAL = 0x4;
	constexpr DWORD MF_GRAYED = 0x1, MF_DISABLED = 0x2, MF_CHECKED = 0x8, MF_POPUP = 0x10;
}

// defaults for programs that build ResDialog.cpp without the Orbiter core, whose own definitions take precedence
__attribute__((weak)) void *ModuleProc (void *hModule, const char *name) { return dlsym (hModule, name); } // not upstream
__attribute__((weak)) void LogOut_Warning (const char *func, const char *file, int line, const char *msg, ...) // not upstream
{
	va_list ap;
	va_start (ap, msg);
	vfprintf (stderr, msg, ap);
	va_end (ap);
	fputc ('\n', stderr);
}

struct CtrlClass { void *hModule; RESCTRLFACTORY create; };

static std::map<std::string, std::vector<CtrlClass>> &CtrlClasses ()
{
	static std::map<std::string, std::vector<CtrlClass>> classes;
	return classes;
}

static std::string Lower (const char *s)
{
	std::string r (s ? s : "");
	for (auto &c : r) c = (char)tolower ((unsigned char)c);
	return r;
}

static void *ExeHandle ()
{
	static void *h = dlopen (nullptr, RTLD_NOW);
	return h;
}

const RESTABLE *oapiResourceTable (void *hModule)
{
	if (!hModule || hModule == ExeHandle())
		return OrbiterResources();
	const RESTABLE *(*table)() = (const RESTABLE*(*)())ModuleProc (hModule, "oapiModuleResources");
	return (table ? table() : nullptr);
}

const RESDIALOG *oapiFindResDialog (void *hModule, int resId)
{
	const RESTABLE *t = oapiResourceTable (hModule);
	if (t)
		for (size_t i = 0; i < t->ndlg; i++)
			if (t->dlg[i].id == resId) return t->dlg + i;
	return nullptr;
}

const RESIMAGE *oapiFindResImage (void *hModule, int resId)
{
	const RESTABLE *t = oapiResourceTable (hModule);
	if (t)
		for (size_t i = 0; i < t->nimg; i++)
			if (t->img[i].id == resId) return t->img + i;
	return nullptr;
}

const RESMENU *oapiFindResMenu (void *hModule, int resId)
{
	const RESTABLE *t = oapiResourceTable (hModule);
	if (t)
		for (size_t i = 0; i < t->nmenu; i++)
			if (t->menu[i].id == resId) return t->menu + i;
	return nullptr;
}

const RESDATA *oapiFindResData (void *hModule, const char *type, int resId)
{
	const RESTABLE *t = oapiResourceTable (hModule);
	if (t)
		for (size_t i = 0; i < t->ndata; i++)
			if (t->data[i].id == resId && !strcasecmp (t->data[i].type, type)) return t->data + i;
	return nullptr;
}

int oapiLoadResString (void *hModule, int id, char *buf, int buflen)
{
	const RESTABLE *t = oapiResourceTable (hModule);
	if (!t || !buf || buflen < 1) return 0;
	for (size_t i = 0; i < t->nstr; i++)
		if (t->str[i].id == id) {
			int n = std::min ((int)strlen (t->str[i].text), buflen-1);
			memcpy (buf, t->str[i].text, n);
			buf[n] = '\0';
			return n;
		}
	buf[0] = '\0';
	return 0;
}

int LoadModuleString (const char *modulefile, int id, char *buf, int buflen)
{
	if (!buf || buflen < 1) return 0;
	buf[0] = '\0';
	std::ifstream f (modulefile, std::ios::binary);
	if (!f) return 0;
	Elf64_Ehdr eh;
	if (!f.read ((char*)&eh, sizeof(eh)) || memcmp (eh.e_ident, ELFMAG, SELFMAG) || eh.e_ident[EI_CLASS] != ELFCLASS64 ||
		eh.e_shentsize != sizeof(Elf64_Shdr) || eh.e_shstrndx >= eh.e_shnum) return 0;
	std::vector<Elf64_Shdr> sh (eh.e_shnum);
	f.seekg (eh.e_shoff);
	if (!f.read ((char*)sh.data(), eh.e_shnum * sizeof(Elf64_Shdr))) return 0;
	std::string names (sh[eh.e_shstrndx].sh_size, '\0');
	f.seekg (sh[eh.e_shstrndx].sh_offset);
	if (!f.read (names.data(), names.size())) return 0;
	for (const Elf64_Shdr &s : sh) {
		if (s.sh_name >= names.size() || strcmp (names.c_str() + s.sh_name, ".oapi_strtab") || s.sh_type == SHT_NOBITS) continue;
		std::string data (s.sh_size, '\0');
		f.seekg (s.sh_offset);
		if (!f.read (data.data(), data.size()) || data.compare (0, 8, "OAPISTR1")) return 0;
		auto u32 = [&data](size_t p) {
			return (uint32_t)(unsigned char)data[p] | (uint32_t)(unsigned char)data[p+1] << 8 |
				(uint32_t)(unsigned char)data[p+2] << 16 | (uint32_t)(unsigned char)data[p+3] << 24;
		};
		for (size_t p = 8; p + 8 <= data.size(); ) {
			uint32_t sid = u32 (p), len = u32 (p+4);
			p += 8;
			if (p + len > data.size()) break;
			if ((int)sid == id) {
				int n = std::min ((int)len, buflen-1);
				memcpy (buf, data.data() + p, n);
				buf[n] = '\0';
				return n;
			}
			p += len;
		}
		return 0;
	}
	return 0;
}

void oapiConnectDlgCommands (QWidget *hDlg, RESCOMMAND handler)
{
	for (QObject *o : hDlg->children()) {
		if (QMenuBar *mb = qobject_cast<QMenuBar*> (o)) { // menu commands: WM_COMMAND with notification code 0
			for (QAction *a : mb->findChildren<QAction*>()) {
				if (!a->property ("resId").isValid()) continue;
				int id = a->property ("resId").toInt();
				QObject::connect (a, &QAction::triggered, hDlg, [handler, id]() { handler (id, RESN_CLICKED, nullptr); });
			}
			continue;
		}
		QWidget *w = qobject_cast<QWidget*> (o);
		if (!w || !w->property ("resId").isValid()) continue;
		int id = w->property ("resId").toInt();
		if (QAbstractButton *b = qobject_cast<QAbstractButton*> (w)) {
			QObject::connect (b, &QAbstractButton::clicked, hDlg, [handler, id, w]() { handler (id, RESN_CLICKED, w); });
		} else if (QLineEdit *e = qobject_cast<QLineEdit*> (w)) {
			QObject::connect (e, &QLineEdit::textChanged, hDlg, [handler, id, w]() { handler (id, RESN_CHANGE, w); });
			QObject::connect (e, &QLineEdit::editingFinished, hDlg, [handler, id, w]() { handler (id, RESN_KILLFOCUS, w); });
		} else if (QPlainTextEdit *e = qobject_cast<QPlainTextEdit*> (w)) {
			QObject::connect (e, &QPlainTextEdit::textChanged, hDlg, [handler, id, w]() { handler (id, RESN_CHANGE, w); });
		} else if (QComboBox *c = qobject_cast<QComboBox*> (w)) {
			QObject::connect (c, &QComboBox::activated, hDlg, [handler, id, w]() { handler (id, RESN_SELCHANGE, w); });
			if (c->isEditable())
				QObject::connect (c, &QComboBox::editTextChanged, hDlg, [handler, id, w]() { handler (id, RESN_EDITCHANGE, w); });
		} else if (QListWidget *l = qobject_cast<QListWidget*> (w)) {
			QObject::connect (l, &QListWidget::itemSelectionChanged, hDlg, [handler, id, w]() { handler (id, RESN_SELCHANGE, w); });
			QObject::connect (l, &QListWidget::itemDoubleClicked, hDlg, [handler, id, w]() { handler (id, RESN_DBLCLK, w); });
		}
	}
}

void oapiConnectDlgDeltaPos (QWidget *hDlg, RESDELTAPOS handler)
{
	for (QObject *o : hDlg->children()) {
		ResUpDown *ud = qobject_cast<ResUpDown*> (o);
		if (!ud || !ud->property ("resId").isValid()) continue;
		int id = ud->property ("resId").toInt();
		QObject::connect (ud, &ResUpDown::deltaPos, hDlg, [handler, id, ud](int d) { handler (id, ud->Pos(), d); });
	}
}

void oapiSetUpDownRange (QWidget *hCtrl, int lower, int upper)
{
	if (ResUpDown *ud = qobject_cast<ResUpDown*> (hCtrl)) ud->SetRange (lower, upper);
}

void oapiSetUpDownPos (QWidget *hCtrl, int pos)
{
	if (ResUpDown *ud = qobject_cast<ResUpDown*> (hCtrl)) ud->SetPos (pos);
}

// UTF-8, or Latin-1 if the bytes are not valid UTF-8 (Windows-era files)
static QString DlgString (const char *text)
{
	QByteArray b (text ? text : "");
	QStringDecoder dec (QStringDecoder::Utf8);
	QString s = dec (b);
	if (dec.hasError()) s = QString::fromLatin1 (b);
	s.remove ('\r');
	return s;
}

void oapiSetDlgText (QWidget *hWnd, const char *text)
{
	if (!hWnd) return;
	QString s = DlgString (text);
	if (QLabel *w = qobject_cast<QLabel*> (hWnd)) w->setText (s);
	else if (QLineEdit *w = qobject_cast<QLineEdit*> (hWnd)) w->setText (s);
	else if (QPlainTextEdit *w = qobject_cast<QPlainTextEdit*> (hWnd)) w->setPlainText (s);
	else if (QTextEdit *w = qobject_cast<QTextEdit*> (hWnd)) w->setPlainText (s);
	else if (QAbstractButton *w = qobject_cast<QAbstractButton*> (hWnd)) w->setText (s);
	else if (QGroupBox *w = qobject_cast<QGroupBox*> (hWnd)) w->setTitle (s);
	else if (QComboBox *w = qobject_cast<QComboBox*> (hWnd)) { if (w->isEditable()) w->setEditText (s); }
	else if (hWnd->isWindow()) hWnd->setWindowTitle (s);
}

int oapiGetDlgText (QWidget *hWnd, char *buf, int buflen)
{
	if (!buf || buflen < 1) return 0;
	buf[0] = '\0';
	if (!hWnd) return 0;
	QString s;
	if (QLabel *w = qobject_cast<QLabel*> (hWnd)) s = w->text();
	else if (QLineEdit *w = qobject_cast<QLineEdit*> (hWnd)) s = w->text();
	else if (QPlainTextEdit *w = qobject_cast<QPlainTextEdit*> (hWnd)) s = w->toPlainText();
	else if (QTextEdit *w = qobject_cast<QTextEdit*> (hWnd)) s = w->toPlainText();
	else if (QAbstractButton *w = qobject_cast<QAbstractButton*> (hWnd)) s = w->text();
	else if (QGroupBox *w = qobject_cast<QGroupBox*> (hWnd)) s = w->title();
	else if (QComboBox *w = qobject_cast<QComboBox*> (hWnd)) s = w->currentText();
	else if (hWnd->isWindow()) s = hWnd->windowTitle();
	QByteArray b = s.toUtf8();
	int n = std::min ((int)b.size(), buflen-1);
	memcpy (buf, b.constData(), n);
	buf[n] = '\0';
	return n;
}

void oapiSetDlgItemText (QWidget *hDlg, int id, const char *text)
{
	oapiSetDlgText (oapiResDlgItem (hDlg, id), text);
}

int oapiGetDlgItemText (QWidget *hDlg, int id, char *buf, int buflen)
{
	return oapiGetDlgText (oapiResDlgItem (hDlg, id), buf, buflen);
}

int oapiComboAddString (QComboBox *cb, const char *str)
{
	QString s = DlgString (str);
	int idx = cb->count();
	if (cb->property ("resSort").toBool()) {
		for (idx = 0; idx < cb->count(); idx++)
			if (QString::compare (s, cb->itemText (idx), Qt::CaseInsensitive) < 0) break;
	}
	cb->insertItem (idx, s);
	return idx;
}

QImage *oapiLoadResImage (void *hModule, int resId)
{
	const RESIMAGE *ri = oapiFindResImage (hModule, resId);
	if (!ri) return nullptr;
	QImage img;
	if (!img.loadFromData (ri->data, ri->size)) return nullptr;
	return new QImage (img);
}

void oapiClearImageBackground (QImage *img)
{
	if (!img || img->isNull()) return;
	*img = img->convertToFormat (QImage::Format_ARGB32);
	const int w = img->width(), h = img->height();
	const QRgb bg = img->pixel (0, 0);
	const int bc[3] = {qRed (bg), qGreen (bg), qBlue (bg)};
	std::vector<char> seen (w*h, 0);
	std::vector<int> todo;
	for (int x = 0; x < w; x++) { todo.push_back (x); todo.push_back ((h-1)*w + x); }
	for (int y = 0; y < h; y++) { todo.push_back (y*w); todo.push_back (y*w + w-1); }
	while (!todo.empty()) {
		int i = todo.back(); todo.pop_back();
		if (seen[i]) continue;
		seen[i] = 1;
		int x = i % w, y = i / w;
		QRgb p = img->pixel (x, y);
		int pc[3] = {qRed (p), qGreen (p), qBlue (p)};
		double a = 0.0; // share of the pixel's own colour; the rest is the surround it was blended with
		bool near = true;
		for (int c = 0; c < 3; c++) {
			if (std::abs (pc[c]-bc[c]) > 75) near = false;
			if (pc[c] < bc[c]) a = std::max (a, (bc[c]-pc[c]) / (double)bc[c]);
		}
		if (!near) continue;
		if (a < 1.0/255.0) img->setPixel (x, y, qRgba (0, 0, 0, 0));
		else {
			int f[3];
			for (int c = 0; c < 3; c++) f[c] = std::clamp ((int)std::lround ((pc[c] - (1.0-a)*bc[c]) / a), 0, 255);
			img->setPixel (x, y, qRgba (f[0], f[1], f[2], (int)std::lround (a*255.0)));
		}
		if (x > 0) todo.push_back (i-1);
		if (x < w-1) todo.push_back (i+1);
		if (y > 0) todo.push_back (i-w);
		if (y < h-1) todo.push_back (i+w);
	}
}

// LoadIcon counterpart for SS_ICON: the SM_CXICON (32x32) image of the .ico, else the first one scaled to it
static QImage *LoadResIcon (void *hModule, int resId)
{
	const RESIMAGE *ri = oapiFindResImage (hModule, resId);
	if (!ri) return nullptr;
	QByteArray ba = QByteArray::fromRawData ((const char*)ri->data, ri->size);
	QBuffer buf (&ba);
	buf.open (QIODevice::ReadOnly);
	QImageReader rd (&buf);
	QImage first;
	for (int i = 0; i < std::max (1, rd.imageCount()); i++) {
		if (i && !rd.jumpToImage (i)) break;
		QImage img = rd.read();
		if (img.isNull()) break;
		if (img.size() == QSize (32, 32)) return new QImage (img);
		if (first.isNull()) first = img;
	}
	if (first.isNull()) return nullptr;
	return new QImage (first.scaled (32, 32, Qt::IgnoreAspectRatio, Qt::SmoothTransformation));
}

void oapiRegisterResControl (void *hModule, const char *cls, RESCTRLFACTORY create)
{
	oapiUnregisterResControl (hModule, cls);
	CtrlClasses()[Lower (cls)].push_back ({hModule, create});
}

void oapiUnregisterResControl (void *hModule, const char *cls)
{
	auto it = CtrlClasses().find (Lower (cls));
	if (it == CtrlClasses().end()) return;
	std::erase_if (it->second, [hModule](const CtrlClass &c) { return c.hModule == hModule; });
	if (it->second.empty()) CtrlClasses().erase (it);
}

static RESCTRLFACTORY FindCtrlClass (void *hModule, const char *cls)
{
	auto it = CtrlClasses().find (Lower (cls));
	if (it == CtrlClasses().end()) return nullptr;
	for (auto &c : it->second)
		if (c.hModule == hModule) return c.create;
	return it->second.back().create;
}

QWidget *oapiResDlgItem (QWidget *hDlg, int id)
{
	if (!hDlg) return nullptr;
	for (QObject *o : hDlg->children()) {
		QWidget *w = qobject_cast<QWidget*> (o);
		if (!w) continue;
		QVariant v = w->property ("resId");
		if (v.isValid() && v.toInt() == id) return w;
	}
	return nullptr;
}

int oapiResId (const QWidget *hWnd)
{
	return (hWnd ? hWnd->property ("resId").toInt() : 0);
}

// dialog font: the .rc system faces map to the desktop UI font at the .rc point size
static QFont DialogFont (const RESDIALOG *d)
{
	static const char *sysfaces[] = {"MS Shell Dlg", "MS Shell Dlg 2", "MS Sans Serif", "Microsoft Sans Serif", "Tahoma", "Segoe UI", "System"};
	QFont f = QApplication::font();
	bool sys = false;
	for (const char *face : sysfaces)
		if (!strcasecmp (d->font, face)) sys = true;
	if (!sys) f.setFamily (QString::fromUtf8 (d->font));
	f.setPointSize (d->fontsize);
	f.setBold (d->weight >= 700);
	f.setItalic (d->italic != 0);
	return f;
}

// GetDialogBaseUnits counterpart for the dialog font: average character width (unrounded, so boxes follow the text width) and height
static void DialogBaseUnits (const QFont &font, double &bx, int &by)
{
	QFontMetrics fm (font);
	bx = fm.horizontalAdvance ("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz") / 52.0;
	by = fm.height();
}

// static control text: '&x' marks an accelerator (not shown), '&&' is a literal '&'
static QString StaticText (const char *text, DWORD style)
{
	QString s = QString::fromUtf8 (text ? text : "");
	s.remove ('\r');
	if (style & rs::SS_NOPREFIX) return s;
	QString r;
	for (int i = 0; i < s.size(); i++) {
		if (s[i] == '&') {
			if (i+1 < s.size() && s[i+1] == '&') { r += '&'; i++; }
			continue;
		}
		r += s[i];
	}
	return r;
}

static QString ButtonText (const char *text)
{
	QString s = QString::fromUtf8 (text ? text : "");
	s.remove ('\r');
	return s;
}

static void SetFill (QWidget *w, const QColor &col)
{
	QPalette pal = w->palette();
	pal.setColor (QPalette::Window, col);
	w->setPalette (pal);
	w->setAutoFillBackground (true);
}

// creates one control; r is its rectangle in pixels (may be adjusted), prev the control created before it
// a Win32 tree view without TVS_FULLROWSELECT highlights the selected item's label only, not its icon or indent
class TreeLabelStyle: public QProxyStyle {
public:
	using QProxyStyle::QProxyStyle;
	int styleHint (StyleHint hint, const QStyleOption *opt, const QWidget *w, QStyleHintReturn *ret) const override
	{
		if (hint == SH_ItemView_ShowDecorationSelected) return 0;
		return QProxyStyle::styleHint (hint, opt, w, ret);
	}
	void drawPrimitive (PrimitiveElement pe, const QStyleOption *opt, QPainter *p, const QWidget *w) const override
	{
		const QStyleOptionViewItem *vo = qstyleoption_cast<const QStyleOptionViewItem*> (opt);
		if (pe != PE_PanelItemViewItem || !vo || !(vo->state & State_Selected)) {
			QProxyStyle::drawPrimitive (pe, opt, p, w);
			return;
		}
		QStyleOptionViewItem o (*vo);
		o.state &= ~State_Selected;
		QProxyStyle::drawPrimitive (pe, &o, p, w);
		QRect r = subElementRect (SE_ItemViewItemText, vo, w);
		int m = pixelMetric (PM_FocusFrameHMargin, vo, w) + 1;
		r.setWidth (std::min (r.width(), vo->fontMetrics.horizontalAdvance (vo->text) + 2*m));
		QPalette::ColorGroup cg = !(vo->state & State_Enabled) ? QPalette::Disabled : (vo->state & State_Active) ? QPalette::Active : QPalette::Inactive;
		p->fillRect (r, vo->palette.brush (cg, QPalette::Highlight));
	}
};

static QWidget *CreateControl (const RESCONTROL *c, QWidget *dlg, void *hModule, QRect &r, QButtonGroup *&radiogroup, QWidget *prev)
{
	using namespace rs;
	QWidget *w = nullptr;
	DWORD st = c->style;
	bool border = (st & WS_BORDER) || (c->exstyle & (WS_EX_CLIENTEDGE | WS_EX_STATICEDGE));

	if (c->kind != RES_RADIOBUTTON && (st & WS_GROUP)) radiogroup = nullptr;

	switch (c->kind) {
	case RES_STATIC: {
		QLabel *l = new QLabel (StaticText (c->text, st), dlg);
		DWORD t = st & 0x1F;
		Qt::Alignment h = (t == SS_CENTER ? Qt::AlignHCenter : t == SS_RIGHT ? Qt::AlignRight : Qt::AlignLeft);
		l->setAlignment (h | ((st & SS_CENTERIMAGE) ? Qt::AlignVCenter : Qt::AlignTop));
		l->setWordWrap (t != SS_SIMPLE && t != SS_LEFTNOWORDWRAP);
		if (st & SS_SUNKEN) l->setFrameStyle (QFrame::Panel | QFrame::Sunken);
		w = l;
		} break;
	case RES_STATICIMAGE: {
		QLabel *l = new QLabel (dlg);
		QImage *img = ((st & 0x1F) == SS_ICON && !(st & SS_REALSIZEIMAGE) ? LoadResIcon (hModule, c->imgid) : oapiLoadResImage (hModule, c->imgid));
		if (img) {
			l->setPixmap (QPixmap::fromImage (*img));
			if (!(st & (SS_REALSIZECONTROL | SS_CENTERIMAGE))) r.setSize (img->size()); // SS_BITMAP sizes the control to the image
			delete img;
		}
		if (st & SS_CENTERIMAGE) l->setAlignment (Qt::AlignCenter);
		w = l;
		} break;
	case RES_STATICFRAME: {
		QFrame *f = new QFrame (dlg);
		switch (st & 0x1F) {
		case SS_BLACKRECT: SetFill (f, Qt::black); break;
		case SS_GRAYRECT:  SetFill (f, Qt::gray); break;
		case SS_WHITERECT: SetFill (f, Qt::white); break;
		case SS_BLACKFRAME: case SS_GRAYFRAME: case SS_WHITEFRAME: f->setFrameStyle (QFrame::Box | QFrame::Plain); break;
		case SS_ETCHEDHORZ: f->setFrameStyle (QFrame::HLine | QFrame::Sunken); break;
		case SS_ETCHEDVERT: f->setFrameStyle (QFrame::VLine | QFrame::Sunken); break;
		case SS_ETCHEDFRAME: f->setFrameStyle (QFrame::Box | QFrame::Sunken); break;
		default: break; // SS_OWNERDRAW: the owner paints it
		}
		if (st & SS_SUNKEN) f->setFrameStyle (QFrame::Panel | QFrame::Sunken);
		w = f;
		} break;
	case RES_BUTTON: {
		QPushButton *b = new QPushButton (ButtonText (c->text), dlg);
		if ((st & 0xF) == BS_DEFPUSHBUTTON) b->setDefault (true);
		if (st & BS_FLAT) b->setFlat (true);
		DWORD ha = st & 0x300;
		if (ha == BS_LEFT) b->setStyleSheet ("text-align: left; padding-left: 4px;");
		else if (ha == BS_RIGHT) b->setStyleSheet ("text-align: right; padding-right: 4px;");
		w = b;
		} break;
	case RES_CHECKBOX:
		if (st & BS_PUSHLIKE) {
			QPushButton *b = new QPushButton (ButtonText (c->text), dlg);
			b->setCheckable (true);
			w = b;
		} else {
			QCheckBox *b = new QCheckBox (ButtonText (c->text), dlg);
			DWORD t = st & 0xF;
			b->setTristate (t == BS_3STATE || t == BS_AUTO3STATE);
			w = b;
		}
		break;
	case RES_RADIOBUTTON: {
		QAbstractButton *b;
		if (st & BS_PUSHLIKE) { b = new QPushButton (ButtonText (c->text), dlg); b->setCheckable (true); }
		else b = new QRadioButton (ButtonText (c->text), dlg);
		b->setAutoExclusive (false);
		if (!radiogroup || (st & WS_GROUP)) { // a WS_GROUP control starts a new radio group
			radiogroup = new QButtonGroup (dlg);
			radiogroup->setExclusive (true);
		}
		radiogroup->addButton (b);
		w = b;
		} break;
	case RES_GROUPBOX: {
		QGroupBox *g = new QGroupBox (ButtonText (c->text), dlg);
		g->setAttribute (Qt::WA_TransparentForMouseEvents); // a Win32 group box is only a frame, the controls are its siblings
		w = g;
		} break;
	case RES_EDIT:
		if (st & ES_MULTILINE) {
			QPlainTextEdit *e = new QPlainTextEdit (dlg);
			e->setReadOnly (st & ES_READONLY);
			e->setLineWrapMode ((st & ES_AUTOHSCROLL) ? QPlainTextEdit::NoWrap : QPlainTextEdit::WidgetWidth);
			e->setVerticalScrollBarPolicy ((st & WS_VSCROLL) ? Qt::ScrollBarAsNeeded : Qt::ScrollBarAlwaysOff);
			e->setHorizontalScrollBarPolicy ((st & WS_HSCROLL) ? Qt::ScrollBarAsNeeded : Qt::ScrollBarAlwaysOff);
			if (!border) e->setFrameStyle (QFrame::NoFrame);
			w = e;
		} else {
			QLineEdit *e = new QLineEdit (dlg);
			e->setReadOnly (st & ES_READONLY);
			if (st & ES_PASSWORD) e->setEchoMode (QLineEdit::Password);
			if (st & ES_NUMBER) e->setValidator (new QRegularExpressionValidator (QRegularExpression ("\\d*"), e));
			DWORD a = st & 0x3;
			e->setAlignment (a == ES_CENTER ? Qt::AlignHCenter : a == ES_RIGHT ? Qt::AlignRight : Qt::AlignLeft);
			e->setFrame (border);
			w = e;
		}
		break;
	case RES_COMBOBOX: {
		QComboBox *cb = new QComboBox (dlg);
		DWORD t = st & 0x3;
		cb->setEditable (t == CBS_DROPDOWN || t == CBS_SIMPLE);
		cb->setProperty ("resSort", (bool)(st & CBS_SORT));
		r.setHeight (cb->sizeHint().height()); // the .rc height includes the dropped-down list
		w = cb;
		} break;
	case RES_LISTBOX: {
		QListWidget *lb = new QListWidget (dlg);
		lb->setSortingEnabled (st & LBS_SORT);
		if (st & LBS_NOSEL) lb->setSelectionMode (QAbstractItemView::NoSelection);
		else if (st & LBS_EXTENDEDSEL) lb->setSelectionMode (QAbstractItemView::ExtendedSelection);
		else if (st & LBS_MULTIPLESEL) lb->setSelectionMode (QAbstractItemView::MultiSelection);
		if (!border) lb->setFrameStyle (QFrame::NoFrame);
		w = lb;
		} break;
	case RES_SCROLLBAR:
		w = new QScrollBar ((st & SBS_VERT) ? Qt::Vertical : Qt::Horizontal, dlg);
		break;
	case RES_UPDOWN: {
		ResUpDown *ud = new ResUpDown (dlg, st & UDS_HORZ, st & UDS_SETBUDDYINT, st & UDS_WRAP);
		if ((st & UDS_AUTOBUDDY) && prev) {
			ud->SetBuddy (prev);
			if (st & (UDS_ALIGNRIGHT | UDS_ALIGNLEFT)) { // attach to the buddy, which gives up the space
				QRect b = prev->geometry();
				int uw = r.width();
				if (st & UDS_ALIGNRIGHT) {
					r = QRect (b.right() - uw + 1, b.top(), uw, b.height());
					prev->setGeometry (b.left(), b.top(), b.width() - uw, b.height());
				} else {
					r = QRect (b.left(), b.top(), uw, b.height());
					prev->setGeometry (b.left() + uw, b.top(), b.width() - uw, b.height());
				}
			}
		}
		w = ud;
		} break;
	case RES_TRACKBAR: {
		QSlider *s = new QSlider ((st & TBS_VERT) ? Qt::Vertical : Qt::Horizontal, dlg);
		s->setRange (0, 100);
		if (st & TBS_VERT) s->setInvertedAppearance (true); // Win32 trackbars have the minimum at the top
		if (st & TBS_NOTICKS) s->setTickPosition (QSlider::NoTicks);
		else if (st & TBS_BOTH) s->setTickPosition (QSlider::TicksBothSides);
		else if (st & TBS_TOP) s->setTickPosition ((st & TBS_VERT) ? QSlider::TicksLeft : QSlider::TicksAbove);
		else s->setTickPosition ((st & TBS_VERT) ? QSlider::TicksRight : QSlider::TicksBelow);
		if (st & TBS_AUTOTICKS) s->setTickInterval (1);
		w = s;
		} break;
	case RES_TREEVIEW: {
		QTreeWidget *tv = new QTreeWidget (dlg);
		tv->setHeaderHidden (true);
		tv->setColumnCount (1);
		tv->setRootIsDecorated (st & TVS_LINESATROOT);
		tv->setProperty ("resCheckboxes", (bool)(st & TVS_CHECKBOXES));
		if (!border) tv->setFrameStyle (QFrame::NoFrame);
		tv->setTextElideMode (Qt::ElideNone); // Win32 trees clip long labels, and scroll sideways unless TVS_NOHSCROLL
		if (!(st & TVS_NOHSCROLL)) {
			tv->header()->setSectionResizeMode (QHeaderView::ResizeToContents);
			tv->header()->setStretchLastSection (false);
			tv->setHorizontalScrollMode (QAbstractItemView::ScrollPerPixel);
		}
		if (!(st & TVS_FULLROWSELECT)) {
			QProxyStyle *ls = new TreeLabelStyle (QStyleFactory::create (QApplication::style()->name()));
			ls->setParent (tv);
			tv->setStyle (ls);
		}
		w = tv;
		} break;
	case RES_TABCONTROL:
		w = new ResTabControl (dlg);
		break;
	case RES_PROGRESS: {
		QProgressBar *p = new QProgressBar (dlg);
		p->setRange (0, 100);
		p->setTextVisible (false);
		if (st & PBS_VERTICAL) p->setOrientation (Qt::Vertical);
		w = p;
		} break;
	case RES_RICHEDIT:
		if (st & ES_READONLY) w = new QTextBrowser (dlg);
		else w = new QTextEdit (dlg);
		break;
	case RES_LISTVIEW: {
		QTreeWidget *lv = new QTreeWidget (dlg);
		lv->setRootIsDecorated (false);
		w = lv;
		} break;
	default: {
		RESCTRLFACTORY create = FindCtrlClass (hModule, c->cls);
		if (create) w = create (c, dlg);
		if (!w) {
			LOGOUT_WARN ("Dialog control class %s is not registered; showing an empty area", c->cls ? c->cls : "(none)");
			w = new QWidget (dlg);
		}
		} break;
	}
	return w;
}

// wrapped multi-line labels that need more lines in this font grow down into free space; in a window the rows below make room
static void FitLabelHeight (QWidget *dlg, const std::vector<std::pair<const RESCONTROL*, QWidget*>> &ctl, QLabel *l)
{
	QRect g = l->geometry();
	int need = l->heightForWidth (g.width());
	if (need <= g.height()) return;
	int blimit = dlg->height() - 2;
	for (auto &[oc, o] : ctl) {
		if (o == l) continue;
		QRect og = o->geometry();
		if (oc->kind == RES_GROUPBOX) {
			if (og.contains (g.center()) && og.bottom() > g.bottom()) blimit = std::min (blimit, og.bottom() - 4);
		} else if (og.left() < g.right() && og.right() > g.left() && og.top() > g.top())
			blimit = std::min (blimit, og.top() - 2);
	}
	int bottom = g.top() + need - 1;
	if (bottom > blimit && dlg->isWindow()) {
		int d = bottom - blimit, y0 = g.bottom();
		for (auto &[oc, o] : ctl) {
			if (o == l) continue;
			QRect og = o->geometry();
			if (og.top() > y0) o->move (og.x(), og.y() + d);
			else if (og.bottom() > y0) o->resize (og.width(), og.height() + d); // group boxes around the label
		}
		if (dlg->minimumSize() == dlg->maximumSize()) dlg->setFixedSize (dlg->width(), dlg->height() + d);
		else dlg->resize (dlg->width(), dlg->height() + d);
		blimit = bottom;
	}
	g.setBottom (std::max (g.bottom(), std::min (bottom, blimit)));
	l->setGeometry (g);
}

// labels that don't fit their .rc box in this font (and a larger check indicator) grow to the right, then to the left, into free space
static void FitLabels (QWidget *dlg, const std::vector<std::pair<const RESCONTROL*, QWidget*>> &ctl)
{
	using namespace rs;
	QFontMetrics fm (dlg->font());
	for (auto &[c, w] : ctl) {
		int need;
		if ((c->kind == RES_CHECKBOX || c->kind == RES_RADIOBUTTON) && !(c->style & BS_PUSHLIKE))
			need = w->sizeHint().width();
		else if (c->kind == RES_STATIC && (c->style & 0x1F) <= SS_RIGHT) {
			QLabel *l = static_cast<QLabel*>(w);
			QString s = l->text();
			if (s.contains ('\n') || w->height() >= 2*fm.height()) {
				if (l->wordWrap()) FitLabelHeight (dlg, ctl, l);
				continue;
			}
			need = fm.horizontalAdvance (s) + 2;
		} else continue;
		QRect g = w->geometry();
		if (need <= g.width()) continue;
		int rlimit = dlg->width() - 2, llimit = 1;
		for (auto &[oc, o] : ctl) {
			if (o == w) continue;
			QRect og = o->geometry();
			if (oc->kind == RES_GROUPBOX) {
				if (og.contains (g.center())) {
					if (og.right() > g.right()) rlimit = std::min (rlimit, og.right() - 4);
					if (og.left() < g.left()) llimit = std::max (llimit, og.left() + 4);
				}
			} else if (og.top() < g.bottom() && og.bottom() > g.top()) {
				if (og.left() > g.left()) rlimit = std::min (rlimit, og.left() - 2);
				if (og.right() < g.right()) llimit = std::max (llimit, og.right() + 2);
			}
		}
		int rfree = std::max (0, rlimit - g.right()), lfree = std::max (0, g.left() - llimit);
		int grow = need - g.width();
		DWORD a = (c->kind == RES_STATIC ? c->style & 0x3 : 0);
		if (a == SS_RIGHT) g.setLeft (g.left() - std::min (grow, lfree)); // text keeps its anchor edge
		else if (a == SS_CENTER) {
			int h = std::min ((grow+1)/2, std::min (lfree, rfree));
			g.adjust (-h, 0, h, 0);
		} else {
			int r = std::min (grow, rfree);
			g.setRight (g.right() + r);
			if (r < grow) g.setLeft (g.left() - std::min (grow - r, lfree));
		}
		w->setGeometry (g);
	}
}

// items of a menu template into a menu bar or popup; it: the next item, n: items at this level (-1: up to end)
template<class M> static void AddMenuItems (M *m, const RESMENUITEM *&it, const RESMENUITEM *end, int n)
{
	using namespace rs;
	for (int k = 0; it < end && (n < 0 || k < n); k++) {
		const RESMENUITEM *item = it++;
		if (!item->text) { // MF_MENUBARBREAK/MF_MENUBREAK columns and MF_HELP right alignment left out: Qt menus have neither
			m->addSeparator();
			continue;
		}
		QString text = QString::fromUtf8 (item->text);
		if (item->flags & MF_POPUP) {
			QMenu *sub = m->addMenu (text);
			sub->menuAction()->setEnabled (!(item->flags & (MF_GRAYED | MF_DISABLED)));
			AddMenuItems (sub, it, end, item->nsub);
		} else {
			QAction *a = m->addAction (text); // Qt takes '&' mnemonics and '\t' shortcut text as Win32 does
			a->setProperty ("resId", item->id);
			a->setEnabled (!(item->flags & (MF_GRAYED | MF_DISABLED)));
			if (item->flags & MF_CHECKED) { a->setCheckable (true); a->setChecked (true); }
		}
	}
}

QMenuBar *oapiCreateResMenu (void *hModule, int resId, QWidget *hWnd)
{
	const RESMENU *rm = oapiFindResMenu (hModule, resId);
	if (!rm || !hWnd) return nullptr;
	QMenuBar *bar = new QMenuBar (hWnd);
	const RESMENUITEM *it = rm->item;
	AddMenuItems (bar, it, rm->item + rm->nitem, -1);
	// the menu is outside the client area: the controls keep their places below it, the window grows
	int h = bar->sizeHint().height();
	for (QObject *o : hWnd->children()) {
		QWidget *w = qobject_cast<QWidget*> (o);
		if (w && w != bar && !w->isWindow()) w->move (w->x(), w->y() + h);
	}
	bar->setGeometry (0, 0, hWnd->width(), h);
	if (hWnd->minimumSize() == hWnd->maximumSize()) hWnd->setFixedSize (hWnd->width(), hWnd->height() + h);
	else hWnd->resize (hWnd->width(), hWnd->height() + h);
	return bar;
}

QWidget *oapiCreateResDialog (void *hModule, int resId, QWidget *parent, QWindow *owner)
{
	using namespace rs;
	const RESDIALOG *d = oapiFindResDialog (hModule, resId);
	if (!d) return nullptr;

	QWidget *dlg;
	bool popup = !(d->style & WS_CHILD);
	if (popup) {
		QDialog *qd = new QDialog (parent);
		Qt::WindowFlags fl = (d->exstyle & WS_EX_APPWINDOW) ? Qt::Window : (d->exstyle & WS_EX_TOOLWINDOW) ? Qt::Tool : Qt::Dialog;
		if (!(d->style & WS_CAPTION)) fl |= Qt::FramelessWindowHint;
		else {
			fl |= Qt::WindowTitleHint | Qt::WindowCloseButtonHint;
			if (d->style & WS_MINIMIZEBOX) fl |= Qt::WindowMinimizeButtonHint;
			if (d->style & WS_MAXIMIZEBOX) fl |= Qt::WindowMaximizeButtonHint;
		}
		if (d->exstyle & WS_EX_TOPMOST) fl |= Qt::WindowStaysOnTopHint;
		qd->setWindowFlags (fl);
		dlg = qd;
	} else {
		dlg = new QWidget (parent);
		dlg->setAutoFillBackground (true); // a Win32 child dialog paints its own background (WM_CTLCOLORDLG), whatever its parent shows
	}
	QFont font = DialogFont (d);
	dlg->setFont (font);
	double bx;
	int by;
	DialogBaseUnits (font, bx, by);
	auto px = [bx](int v) { return (int)std::lround (v * bx / 4.0); }; // MapDialogRect
	auto py = [by](int v) { return (int)std::lround (v * by / 8.0); };

	dlg->setObjectName (QString::fromUtf8 (d->name));
	dlg->setProperty ("resId", d->id);
	dlg->setProperty ("resModule", QVariant::fromValue ((void*)hModule));
	dlg->setProperty ("resBaseX", bx);
	dlg->setProperty ("resBaseY", by);
	if (d->caption && d->caption[0]) dlg->setWindowTitle (QString::fromUtf8 (d->caption));

	QButtonGroup *radiogroup = nullptr;
	QWidget *prev = nullptr;
	std::vector<std::pair<const RESCONTROL*, QWidget*>> ctl;
	for (int i = 0; i < d->nctrl; i++) {
		const RESCONTROL *c = d->ctrl + i;
		QRect r (px(c->x), py(c->y), px(c->cx), py(c->cy));
		QWidget *w = CreateControl (c, dlg, hModule, r, radiogroup, prev);
		w->setObjectName (c->idname ? QString::fromUtf8 (c->idname) : QString ("id%1").arg (c->id));
		w->setProperty ("resId", c->id);
		w->setGeometry (r);
		if (c->style & WS_DISABLED) w->setEnabled (false);
		if (!(c->style & WS_VISIBLE)) w->hide();
		ctl.push_back ({c, w});
		prev = w;
	}
	// Win32 stacks the first control of a template on top; group boxes only frame their siblings
	for (auto it = ctl.rbegin(); it != ctl.rend(); ++it) it->second->raise();
	for (auto &[c, w] : ctl)
		if (c->kind == RES_GROUPBOX) w->lower();

	QSize size (px(d->cx), py(d->cy));
	if (popup) {
		dlg->resize (size);
		if (!(d->style & WS_THICKFRAME)) dlg->setFixedSize (size);
		QWidget *top = parent ? parent->window() : nullptr;
		QRect area = top ? top->geometry() : (QGuiApplication::primaryScreen() ? QGuiApplication::primaryScreen()->availableGeometry() : QRect (0, 0, 1920, 1080));
		if (d->style & DS_CENTER) dlg->move (area.center() - QPoint (size.width()/2, size.height()/2));
		else dlg->move (area.topLeft() + QPoint (px(d->x), py(d->y)));
	} else {
		dlg->setGeometry (px(d->x), py(d->y), size.width(), size.height());
	}
	FitLabels (dlg, ctl);
	if (d->menu >= 0) oapiCreateResMenu (hModule, d->menu, dlg);
	if (d->style & WS_DISABLED) dlg->setEnabled (false);
	if (owner && popup) { // hWndParent: the owner is known before WS_VISIBLE shows the window (Wayland takes it only at the first show)
		dlg->winId();
		if (dlg->windowHandle()) dlg->windowHandle()->setTransientParent (owner);
	}
	if (d->style & WS_VISIBLE) dlg->show();
	return dlg;
}

int oapiExecOwned (QDialog *dlg, QWindow *owner)
{
	if (owner) { // the owner is set before the first show (Wayland takes it only then)
		dlg->winId();
		if (dlg->windowHandle()) dlg->windowHandle()->setTransientParent (owner);
	}
	return dlg->exec();
}

// ======================================================================
// ResUpDown

ResUpDown::ResUpDown (QWidget *parent, bool horizontal, bool setbuddyint, bool wrap)
: QWidget (parent), horz (horizontal), buddyint (setbuddyint), wrap (wrap)
{
	static const Qt::ArrowType arrow[2][2] = {{Qt::UpArrow, Qt::DownArrow}, {Qt::RightArrow, Qt::LeftArrow}};
	for (int i = 0; i < 2; i++) {
		bt[i] = new QToolButton (this);
		bt[i]->setArrowType (arrow[horz ? 1 : 0][i]);
		bt[i]->setAutoRepeat (true);
		bt[i]->setAutoRepeatDelay (400);
		bt[i]->setAutoRepeatInterval (60);
		bt[i]->setFocusPolicy (Qt::NoFocus);
		connect (bt[i], &QToolButton::clicked, this, [this, i]() { Step (i ? -1 : 1); });
	}
}

void ResUpDown::SetBuddy (QWidget *b)
{
	buddy = b;
	if (buddyint) SetPos (pos);
}

void ResUpDown::SetRange (int lower, int upper)
{
	lo = lower, hi = upper;
	SetPos (pos);
}

void ResUpDown::SetPos (int p)
{
	int mn = std::min (lo, hi), mx = std::max (lo, hi);
	pos = std::max (mn, std::min (mx, p));
	if (buddyint && buddy) {
		QString s = QString::number (pos);
		if (QLineEdit *e = qobject_cast<QLineEdit*> (buddy)) e->setText (s);
		else if (QLabel *l = qobject_cast<QLabel*> (buddy)) l->setText (s);
	}
}

// the up/right button moves toward the upper range limit, which may be numerically smaller
void ResUpDown::Step (int dir)
{
	if (buddyint && buddy) { // the user may have typed into the buddy
		bool ok;
		int v = 0;
		if (QLineEdit *e = qobject_cast<QLineEdit*> (buddy)) v = e->text().toInt (&ok);
		else ok = false;
		if (ok) pos = v;
	}
	int delta = (hi >= lo ? dir : -dir);
	emit deltaPos (delta);
	int mn = std::min (lo, hi), mx = std::max (lo, hi);
	int p = pos + delta;
	if (p > mx) p = (wrap ? mn : mx);
	else if (p < mn) p = (wrap ? mx : mn);
	if (p == pos) return;
	delta = p - pos;
	SetPos (p);
	emit valueChanged (pos, delta);
}

void ResUpDown::resizeEvent (QResizeEvent *event)
{
	int w = width(), h = height();
	if (horz) {
		bt[1]->setGeometry (0, 0, w/2, h);
		bt[0]->setGeometry (w/2, 0, w - w/2, h);
	} else {
		bt[0]->setGeometry (0, 0, w, h/2);
		bt[1]->setGeometry (0, h/2, w, h - h/2);
	}
	QWidget::resizeEvent (event);
}

// ======================================================================
// ResTabControl

ResTabControl::ResTabControl (QWidget *parent): QWidget (parent)
{
	bar = new QTabBar (this);
	bar->setDrawBase (false);
	bar->setExpanding (false);
	bar->setFocusPolicy (Qt::TabFocus);
}

QRect ResTabControl::DisplayRect () const
{
	int bh = bar->sizeHint().height();
	return QRect (2, bh + 2, std::max (0, width() - 4), std::max (0, height() - bh - 4));
}

void ResTabControl::resizeEvent (QResizeEvent *event)
{
	bar->setGeometry (0, 0, width(), bar->sizeHint().height());
	QWidget::resizeEvent (event);
}

void ResTabControl::paintEvent (QPaintEvent*)
{
	QPainter p (this);
	QStyleOptionTabWidgetFrame opt;
	opt.initFrom (this);
	int bh = bar->sizeHint().height();
	opt.rect = QRect (0, bh - 1, width(), height() - bh + 1);
	opt.shape = QTabBar::RoundedNorth;
	opt.tabBarSize = bar->sizeHint();
	opt.lineWidth = style()->pixelMetric (QStyle::PM_DefaultFrameWidth, nullptr, this);
	style()->drawPrimitive (QStyle::PE_FrameTabWidget, &opt, &p, this);
}

// not upstream: help viewer for Orbiter's HTML help projects (stands in for the HtmlHelp API and the .chm reader)

#include "ChmHelp.h"
#include "OrbiterAPI.h"
#include <QDesktopServices>
#include <QFile>
#include <QFileInfo>
#include <QHBoxLayout>
#include <QHash>
#include <QPointer>
#include <QRegularExpression>
#include <QResizeEvent>
#include <QScrollBar>
#include <QSplitter>
#include <QStringDecoder>
#include <QTextBlock>
#include <QTextCursor>
#include <QTextDocument>
#include <QTextList>
#include <QTreeWidget>
#include <algorithm>
#include <map>
#include <memory>
#include <string>
#include <zlib.h>

// .chm file: a zip archive (stored or deflated entries); names are found ignoring case, '\' separators and a leading "/" or "./"
class ChmFile {
public:
	bool Load (const QString &path)
	{
		QFile f (path);
		if (!f.open (QIODevice::ReadOnly)) return false;
		data = f.readAll();
		const uchar *d = (const uchar*)data.constData();
		qint64 n = data.size();
		auto u16 = [d](qint64 i) { return (quint32)d[i] | (quint32)d[i+1] << 8; };
		auto u32 = [d](qint64 i) { return (quint32)d[i] | (quint32)d[i+1] << 8 | (quint32)d[i+2] << 16 | (quint32)d[i+3] << 24; };
		qint64 eocd = -1; // end of central directory record
		for (qint64 i = n - 22; i >= 0 && i >= n - 22 - 65535; i--)
			if (u32 (i) == 0x06054b50) { eocd = i; break; }
		if (eocd < 0) return false;
		qint64 p = u32 (eocd + 16);
		quint32 count = u16 (eocd + 10);
		for (quint32 k = 0; k < count && p + 46 <= n && u32 (p) == 0x02014b50; k++) {
			Entry e;
			e.method = u16 (p + 10);
			e.csize = u32 (p + 20);
			e.usize = u32 (p + 24);
			quint32 nlen = u16 (p + 28), xlen = u16 (p + 30), clen = u16 (p + 32);
			qint64 lh = u32 (p + 42);
			if (p + 46 + nlen > n) break;
			QString name = QString::fromUtf8 (data.constData() + p + 46, nlen);
			if (lh + 30 <= n && u32 (lh) == 0x04034b50) {
				e.ofs = lh + 30 + u16 (lh + 26) + u16 (lh + 28);
				if (e.ofs + e.csize <= n) {
					dir[name.toLower()] = e;
					names << name;
				}
			}
			p += 46 + nlen + xlen + clen;
		}
		return !dir.isEmpty();
	}

	QByteArray Read (QString name) const
	{
		name.replace ('\\', '/');
		while (name.startsWith ('/') || name.startsWith ("./")) name.remove (0, name.startsWith ('/') ? 1 : 2);
		auto it = dir.find (name.toLower());
		if (it == dir.end()) return QByteArray();
		const Entry &e = it.value();
		if (e.method == 0) return data.mid (e.ofs, e.csize);
		if (e.method != 8) return QByteArray();
		QByteArray out (e.usize, Qt::Uninitialized);
		z_stream zs = {};
		zs.next_in = (Bytef*)data.constData() + e.ofs;
		zs.avail_in = e.csize;
		zs.next_out = (Bytef*)out.data();
		zs.avail_out = e.usize;
		if (inflateInit2 (&zs, -MAX_WBITS) != Z_OK) return QByteArray();
		int r = inflate (&zs, Z_FINISH);
		inflateEnd (&zs);
		return (r == Z_STREAM_END ? out : QByteArray());
	}

	QStringList names;

private:
	struct Entry { qint64 ofs = 0; quint32 csize = 0, usize = 0, method = 0; };
	QByteArray data;
	QHash<QString, Entry> dir;
};

// help files stay open once read
static std::shared_ptr<ChmFile> OpenChm (const QString &path)
{
	static std::map<QString, std::shared_ptr<ChmFile>> cache;
	QFileInfo fi (path);
	QString key = fi.absoluteFilePath();
	auto it = cache.find (key);
	if (it != cache.end()) return it->second;
	auto chm = std::make_shared<ChmFile>();
	if (!fi.isFile() || !chm->Load (key)) return nullptr;
	cache[key] = chm;
	return chm;
}

// "html\\orbiter.chm" -> the resolved help file, empty if there is none
static QString ChmPath (const std::string &chm)
{
	std::string f = chm;
	for (auto &c : f) if (c == '\\') c = '/';
	QFileInfo fi (QString::fromStdString (oapiResolvePath (f.c_str())));
	return (fi.isFile() ? fi.absoluteFilePath() : QString());
}

QUrl ChmUrl (const QString &chmfile, const QString &topic)
{
	QString t = topic;
	t.replace ('\\', '/');
	while (t.startsWith ('/')) t.remove (0, 1);
	QString frag;
	int h = t.indexOf ('#');
	if (h >= 0) frag = t.mid (h+1), t.truncate (h);
	QUrl url;
	url.setScheme ("chm");
	url.setPath (chmfile + "/" + t);
	if (!frag.isEmpty()) url.setFragment (frag);
	return url;
}

// chm URL -> help file and page
static bool SplitChmUrl (const QUrl &url, QString &chmfile, QString &topic)
{
	QString p = url.path();
	for (int i = p.indexOf (".chm/", 0, Qt::CaseInsensitive); i >= 0; i = p.indexOf (".chm/", i+1, Qt::CaseInsensitive)) {
		if (QFileInfo (p.left (i+4)).isFile()) {
			chmfile = p.left (i+4);
			topic = p.mid (i+5);
			return true;
		}
	}
	return false;
}

// "its:dir\\file.chm::/topic" (also ms-its:, mk:@MSITStore:); relto: folder for a file name without a folder
static QUrl ItsUrl (const QString &its, const QString &relto)
{
	int sep = its.indexOf (".chm::", 0, Qt::CaseInsensitive);
	if (sep < 0) return QUrl();
	QString chm = its.left (sep + 4), topic = its.mid (sep + 6);
	int colon = chm.lastIndexOf (':');
	if (colon > 1) chm = chm.mid (colon + 1);
	if (chm.startsWith ("file://")) chm = QUrl (chm).toLocalFile();
	QString path;
	if (!relto.isEmpty()) path = ChmPath ((relto + "/" + chm).toStdString());
	if (path.isEmpty()) path = ChmPath (chm.toStdString());
	return (path.isEmpty() ? QUrl() : ChmUrl (path, topic));
}

QUrl ChmUrlFromIts (const QString &its)
{
	return ItsUrl (its, QString());
}

// Internet Explorer sizes <img width="N%"> to N% of its containing block; QTextBrowser takes pixel widths only
QString ChmBrowser::PercentImages (const QString &html)
{
	static const QRegularExpression img ("<img\\b[^>]*>", QRegularExpression::CaseInsensitiveOption);
	static const QRegularExpression pct ("\\bwidth\\s*=\\s*[\"']?\\s*([0-9.]+)\\s*%[\"']?", QRegularExpression::CaseInsensitiveOption);
	static const QRegularExpression src ("\\bsrc\\s*=\\s*(?:\"([^\"]*)\"|'([^']*)'|([^\\s>]+))", QRegularExpression::CaseInsensitiveOption);
	pctImages.clear();
	pctWidth = PageWidth();
	QString out;
	qsizetype pos = 0;
	auto it = img.globalMatch (html);
	while (it.hasNext()) {
		QRegularExpressionMatch m = it.next();
		QString tag = m.captured();
		QRegularExpressionMatch w = pct.match (tag);
		QRegularExpressionMatch sm = src.match (tag);
		if (w.hasMatch() && sm.hasMatch()) {
			QString name = sm.captured (1) + sm.captured (2) + sm.captured (3);
			double n = w.captured (1).toDouble();
			pctImages.push_back ({name, n});
			tag.replace (w.capturedStart(), w.capturedLength(), QString ("width=\"%1\"").arg (qRound (n * pctWidth / 100.0))); // first guess: the whole page
		}
		out += html.mid (pos, m.capturedStart() - pos) + tag;
		pos = m.capturedEnd();
	}
	return out + html.mid (pos);
}

// the same width with or without the vertical scroll bar, so the layout can't flip between the two
int ChmBrowser::PageWidth () const
{
	int w = viewport()->width();
	if (!verticalScrollBar()->isVisible()) w -= verticalScrollBar()->sizeHint().width();
	return std::max (1, w - 2*(int)document()->documentMargin());
}

// each percentage image gets its share of the width left in its paragraph (margins, list indents)
void ChmBrowser::FitPercentImages ()
{
	if (pctImages.empty()) return;
	QTextDocument *doc = document();
	pctWidth = PageWidth();
	struct Fit { int pos, len; QTextImageFormat fmt; };
	std::vector<Fit> fits;
	size_t k = 0;
	for (QTextBlock b = doc->begin(); b.isValid() && k < pctImages.size(); b = b.next()) {
		QTextBlockFormat bf = b.blockFormat();
		double indent = bf.indent() + (b.textList() ? b.textList()->format().indent() : 0);
		double avail = pctWidth - bf.leftMargin() - bf.rightMargin() - bf.textIndent() - indent * doc->indentWidth();
		for (auto it = b.begin(); !it.atEnd() && k < pctImages.size(); ++it) {
			QTextFragment f = it.fragment();
			if (!f.isValid() || !f.charFormat().isImageFormat()) continue;
			QTextImageFormat fmt = f.charFormat().toImageFormat();
			if (fmt.name() != pctImages[k].first) continue;
			int px = std::max (1, qRound (pctImages[k++].second * avail / 100.0));
			if (qRound (fmt.width()) == px) continue;
			fmt.setWidth (px);
			fits.push_back ({f.position(), f.length(), fmt});
		}
	}
	QTextCursor cur (doc);
	for (auto &fit: fits) {
		cur.setPosition (fit.pos);
		cur.setPosition (fit.pos + fit.len, QTextCursor::KeepAnchor);
		cur.setCharFormat (fit.fmt);
	}
}

void ChmBrowser::SetPageHtml (const QString &html)
{
	setHtml (PercentImages (html));
	FitPercentImages();
}

void ChmBrowser::doSetSource (const QUrl &name, QTextDocument::ResourceType type)
{
	QTextBrowser::doSetSource (name, type);
	FitPercentImages();
}

void ChmBrowser::resizeEvent (QResizeEvent *e)
{
	QTextBrowser::resizeEvent (e);
	if (!pctImages.empty() && PageWidth() != pctWidth) FitPercentImages();
}

QVariant ChmBrowser::loadResource (int type, const QUrl &name)
{
	QVariant v = LoadPage (type, name);
	if (type != QTextDocument::HtmlResource || v.isNull()) return v;
	QString s;
	if (v.userType() == QMetaType::QString) s = v.toString();
	else {
		QByteArray b = v.toByteArray();
		QStringDecoder dec = QStringDecoder::decoderForHtml (b); // BOM or meta charset
		if (!dec.isValid()) dec = QStringDecoder (QStringDecoder::Utf8);
		s = dec (b);
		if (dec.hasError()) s = QString::fromLatin1 (b); // Windows-era pages
	}
	return PercentImages (s);
}

QVariant ChmBrowser::LoadPage (int type, const QUrl &name)
{
	QString chmfile, topic;
	if (name.scheme() != "chm" || !SplitChmUrl (name, chmfile, topic)) return QTextBrowser::loadResource (type, name);
	auto chm = OpenChm (chmfile);
	QByteArray b = (chm ? chm->Read (topic) : QByteArray());
	if (b.isEmpty()) return QVariant();
	if (type == QTextDocument::HtmlResource || type == QTextDocument::StyleSheetResource) {
		QStringDecoder dec = QStringDecoder::decoderForHtml (b); // BOM or meta charset
		if (!dec.isValid()) dec = QStringDecoder (QStringDecoder::Utf8);
		QString s = dec (b);
		if (dec.hasError()) s = QString::fromLatin1 (b); // Windows-era pages
		return s;
	}
	return b;
}

class HelpWindow: public QWidget {
public:
	HelpWindow (QWidget *owner): QWidget (owner, Qt::Window)
	{
		resize (900, 640);
		QHBoxLayout *layout = new QHBoxLayout (this);
		layout->setContentsMargins (0, 0, 0, 0);
		QSplitter *split = new QSplitter (this);
		toc = new QTreeWidget (split);
		toc->setHeaderHidden (true);
		page = new ChmBrowser (split);
		page->setOpenLinks (false);
		split->addWidget (toc);
		split->addWidget (page);
		split->setStretchFactor (1, 1);
		split->setSizes ({240, 660});
		layout->addWidget (split);
		setAttribute (Qt::WA_DeleteOnClose);
		connect (toc, &QTreeWidget::itemClicked, this, [this](QTreeWidgetItem *item) {
			QString local = item->data (0, Qt::UserRole).toString();
			if (!local.isEmpty()) ShowTopic (local);
		});
		connect (page, &QTextBrowser::anchorClicked, this, [this](const QUrl &url) { FollowLink (url); });
	}

	bool Load (const QString &file)
	{
		if (file == chmfile) return true;
		std::shared_ptr<ChmFile> c = OpenChm (file);
		if (!c) return false;
		chmfile = file;
		toc->clear();
		deftopic.clear();
		QString contents;
		for (const QString &n : c->names) { // project options: title, contents file, default topic
			if (n.contains ('/') || !n.endsWith (".hhp", Qt::CaseInsensitive)) continue;
			for (QString line : QString::fromLatin1 (c->Read (n)).split ('\n')) {
				line = line.trimmed();
				if (line.startsWith ("Title=", Qt::CaseInsensitive)) setWindowTitle (line.mid (6));
				else if (line.startsWith ("Contents file=", Qt::CaseInsensitive)) contents = line.mid (14);
				else if (line.startsWith ("Default topic=", Qt::CaseInsensitive)) deftopic = line.mid (14);
			}
			break;
		}
		if (contents.isEmpty())
			for (const QString &n : c->names)
				if (!n.contains ('/') && n.endsWith (".hhc", Qt::CaseInsensitive)) { contents = n; break; }
		if (!contents.isEmpty()) ReadContents (QString::fromLatin1 (c->Read (contents)));
		return true;
	}

	void ShowTopic (const QString &topic)
	{
		page->setSource (ChmUrl (chmfile, topic.isEmpty() ? deftopic : topic));
	}

private:
	// sitemap: nested <UL> lists of <OBJECT type="text/sitemap"> entries with Name and Local parameters
	void ReadContents (const QString &html)
	{
		static const QRegularExpression tag ("<\\s*(/?)\\s*(UL|OBJECT|param)\\b([^>]*)>", QRegularExpression::CaseInsensitiveOption);
		static const QRegularExpression attr ("(\\w+)\\s*=\\s*\"([^\"]*)\"");
		QList<QTreeWidgetItem*> stack;
		QTreeWidgetItem *last = nullptr;
		QString name, local;
		bool inobj = false;
		for (auto it = tag.globalMatch (html); it.hasNext(); ) {
			auto m = it.next();
			bool close = !m.captured (1).isEmpty();
			QString t = m.captured (2).toLower();
			if (t == "ul") {
				if (!close) stack.push_back (last);
				else if (!stack.isEmpty()) last = stack.takeLast();
			} else if (t == "object") {
				if (!close) {
					inobj = m.captured (3).contains ("text/sitemap", Qt::CaseInsensitive);
					name.clear(), local.clear();
				} else if (inobj) {
					QTreeWidgetItem *parent = (stack.isEmpty() ? nullptr : stack.back());
					QTreeWidgetItem *item = (parent ? new QTreeWidgetItem (parent) : new QTreeWidgetItem (toc));
					item->setText (0, name);
					item->setData (0, Qt::UserRole, local);
					last = item;
					inobj = false;
				}
			} else if (t == "param" && inobj) {
				QString pname, pvalue;
				for (auto a = attr.globalMatch (m.captured (3)); a.hasNext(); ) {
					auto am = a.next();
					if (!am.captured (1).compare ("name", Qt::CaseInsensitive)) pname = am.captured (2);
					else if (!am.captured (1).compare ("value", Qt::CaseInsensitive)) pvalue = am.captured (2);
				}
				if (!pname.compare ("Name", Qt::CaseInsensitive)) name = pvalue;
				else if (!pname.compare ("Local", Qt::CaseInsensitive)) local = pvalue;
			}
		}
	}

	// links into other help files ("other.chm::/topic.htm", "ms-its:other.chm::/topic.htm"), web links, and pages
	void FollowLink (const QUrl &url)
	{
		QString s = url.toString();
		if (s.contains (".chm::", Qt::CaseInsensitive)) {
			QUrl u = ItsUrl (s, QFileInfo (chmfile).absolutePath());
			QString file, topic;
			if (SplitChmUrl (u, file, topic) && Load (file)) ShowTopic (topic);
			return;
		}
		if (url.scheme() == "http" || url.scheme() == "https" || url.scheme() == "mailto") {
			QDesktopServices::openUrl (url);
			return;
		}
		page->setSource (url);
	}

	QTreeWidget *toc;
	ChmBrowser *page;
	QString chmfile;
	QString deftopic;
};

static QPointer<HelpWindow> g_help;

bool HtmlHelp (QWidget *owner, const char *file, const char *topic)
{
	if (!file) return false;
	std::string chm = file, t = (topic ? topic : "");
	size_t sep = chm.find ("::");
	if (sep != std::string::npos) { // "file.chm::/topic.htm"
		if (t.empty()) t = chm.substr (sep + 2);
		chm.resize (sep);
	}
	QString path = ChmPath (chm);
	if (path.isEmpty()) return false;
	if (!g_help) g_help = new HelpWindow (owner ? owner->window() : nullptr);
	if (!g_help->Load (path)) return false;
	g_help->ShowTopic (QString::fromStdString (t));
	g_help->show();
	g_help->raise();
	g_help->activateWindow();
	return true;
}

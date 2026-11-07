// Copyright (c) Martin Schweiger
// Licensed under the MIT License

//=============================================================================
// ScenarioTab class
//=============================================================================

#include <windows.h>
#include <algorithm>
#include <chrono>
#include <fstream>
#include <sstream>
#include <string>
#include "Orbiter.h"
#include "TabScenario.h"
#include "Launchpad.h"
#include "OrbiterPlatform.h"
#include "imgui.h"

extern const TCHAR* CurrentScenario;

namespace {

	std::string Normalise (std::string s)
	{
		for (char& c : s) if (c == '\\') c = '/';
		return s;
	}

	std::string ToLower (std::string s)
	{
		for (char& c : s) c = (char)tolower ((unsigned char)c);
		return s;
	}

	double Now ()
	{
		return std::chrono::duration<double> (std::chrono::steady_clock::now ().time_since_epoch ()).count ();
	}

	bool CaseLess (const std::string& a, const std::string& b)
	{
		return strcasecmp (a.c_str (), b.c_str ()) < 0;
	}

	std::string Trim (const std::string& s)
	{
		size_t a = s.find_first_not_of (" \t\r\n");
		if (a == std::string::npos) return "";
		size_t b = s.find_last_not_of (" \t\r\n");
		return s.substr (a, b - a + 1);
	}

	fs::path FindCI (const fs::path& p)
	{
		std::error_code ec;
		if (p.empty () || fs::exists (p, ec)) return p;
		fs::path parent = p.parent_path ();
		fs::path dir = parent.empty () ? fs::path (".") : FindCI (parent);
		std::string b = p.filename ().string ();
		for (auto& e : fs::directory_iterator (dir, ec)) {
			std::string a = e.path ().filename ().string ();
			if (!strcasecmp (a.c_str (), b.c_str ()))
				return e.path ();
		}
		return parent.empty () ? p : dir / p.filename ();
	}

	bool ReadFile (const fs::path& path, std::string& data)
	{
		std::ifstream ifs (path, std::ios::binary);
		if (!ifs) return false;
		std::stringstream ss;
		ss << ifs.rdbuf ();
		data = ss.str ();
		return true;
	}

	bool ValidUtf8 (const std::string& s)
	{
		const unsigned char* c = (const unsigned char*)s.data ();
		size_t n = s.size (), i = 0;
		while (i < n) {
			unsigned char b = c[i];
			int len = b < 0x80 ? 1 : (b >> 5) == 0x6 ? 2 : (b >> 4) == 0xe ? 3 : (b >> 3) == 0x1e ? 4 : 0;
			if (!len || i + len > n) return false;
			for (int k = 1; k < len; k++)
				if ((c[i + k] & 0xc0) != 0x80) return false;
			i += len;
		}
		return true;
	}

	void AppendUtf8 (std::string& out, unsigned cp)
	{
		if (cp < 0x80) out += (char)cp;
		else if (cp < 0x800) { out += (char)(0xc0 | (cp >> 6)); out += (char)(0x80 | (cp & 0x3f)); }
		else if (cp < 0x10000) { out += (char)(0xe0 | (cp >> 12)); out += (char)(0x80 | ((cp >> 6) & 0x3f)); out += (char)(0x80 | (cp & 0x3f)); }
		else { out += (char)(0xf0 | (cp >> 18)); out += (char)(0x80 | ((cp >> 12) & 0x3f)); out += (char)(0x80 | ((cp >> 6) & 0x3f)); out += (char)(0x80 | (cp & 0x3f)); }
	}

	std::string ToUtf8 (const std::string& s)
	{
		if (ValidUtf8 (s)) return s;
		std::string out;
		for (unsigned char c : s) AppendUtf8 (out, c);
		return out;
	}

	// Extract the contents of a BEGIN_<name> ... END_<name> block
	bool ExtractBlock (const std::string& data, const char* name, std::string& block)
	{
		std::string begin = std::string ("BEGIN_") + name, end = std::string ("END_") + name;
		std::istringstream is (data);
		std::string line;
		bool found = false;
		while (std::getline (is, line)) {
			if (!strcasecmp (Trim (line).c_str (), begin.c_str ())) { found = true; break; }
		}
		if (!found) return false;
		block.clear ();
		while (std::getline (is, line)) {
			if (!line.empty () && line.back () == '\r') line.pop_back ();
			if (!strncasecmp (line.c_str (), end.c_str (), end.size ())) break;
			block += line;
			block += '\n';
		}
		return true;
	}

	std::string DecodeEntities (const std::string& s)
	{
		static const struct { const char* name; unsigned cp; } ent[] = {
			{"amp", '&'}, {"lt", '<'}, {"gt", '>'}, {"quot", '"'}, {"apos", '\''}, {"nbsp", ' '},
			{"ge", 0x2265}, {"le", 0x2264}, {"deg", 0xb0}, {"reg", 0xae}, {"copy", 0xa9}, {"trade", 0x2122},
			{"ndash", 0x2013}, {"mdash", 0x2014}, {"lsquo", 0x2018}, {"rsquo", 0x2019}, {"ldquo", 0x201c},
			{"rdquo", 0x201d}, {"hellip", 0x2026}, {"times", 0xd7}, {"plusmn", 0xb1}, {"micro", 0xb5},
			{"middot", 0xb7}, {"bull", 0x2022}, {"rarr", 0x2192}, {"larr", 0x2190}, {"sup2", 0xb2}, {"sup3", 0xb3},
		};
		std::string out;
		for (size_t i = 0; i < s.size (); i++) {
			if (s[i] == '&') {
				size_t e = s.find (';', i);
				if (e != std::string::npos && e - i <= 10) {
					std::string name = s.substr (i + 1, e - i - 1);
					bool ok = false;
					if (!name.empty () && name[0] == '#') {
						unsigned cp = (name.size () > 1 && (name[1] == 'x' || name[1] == 'X'))
							? (unsigned)strtoul (name.c_str () + 2, 0, 16) : (unsigned)strtoul (name.c_str () + 1, 0, 10);
						if (cp) { AppendUtf8 (out, cp); ok = true; }
					}
					else {
						for (auto& en : ent)
							if (name == en.name) { AppendUtf8 (out, en.cp); ok = true; break; }
					}
					if (ok) { i = e; continue; }
				}
			}
			out += s[i];
		}
		return out;
	}

	std::string Attribute (const std::string& tag, const char* name)
	{
		std::string lt = ToLower (tag);
		std::string key = name;
		size_t p = 0;
		while ((p = lt.find (key, p)) != std::string::npos) {
			bool startOk = p > 0 && isspace ((unsigned char)lt[p - 1]);
			size_t q = p + key.size ();
			while (q < lt.size () && isspace ((unsigned char)lt[q])) q++;
			if (startOk && q < lt.size () && lt[q] == '=') {
				q++;
				while (q < lt.size () && isspace ((unsigned char)lt[q])) q++;
				if (q >= tag.size ()) return "";
				char quote = tag[q];
				if (quote == '"' || quote == '\'') {
					size_t r = tag.find (quote, q + 1);
					return tag.substr (q + 1, r == std::string::npos ? std::string::npos : r - q - 1);
				}
				size_t r = q;
				while (r < tag.size () && !isspace ((unsigned char)tag[r]) && tag[r] != '>' && tag[r] != '/') r++;
				return tag.substr (q, r - q);
			}
			p = q;
		}
		return "";
	}

}

//-----------------------------------------------------------------------------

orbiter::ScenarioTab::ScenarioTab (LaunchpadDialog *lp): LaunchpadTab (lp, "Scenarios", "tab_scenario")
{
	m_root.dir = true;
	m_selDir = false;
	m_reveal = false;
	m_descMode = -1;
	m_lastScan = 0.0;
	m_listWidth = 260.0f;
	m_paused = false;
	m_openSave = false;
	m_openNoState = false;
	m_saveName[0] = '\0';
	m_saveDesc[0] = '\0';
	m_saveExists = false;
	m_openClear = false;
}

//-----------------------------------------------------------------------------

orbiter::ScenarioTab::~ScenarioTab ()
{
	ClearDescription ();
}

//-----------------------------------------------------------------------------

void orbiter::ScenarioTab::GetConfig (const Config *cfg)
{
	m_paused = cfg->CfgLogicPrm.bStartPaused;
	int listw = cfg->CfgWindowPos.LaunchpadScnListWidth;
	m_listWidth = listw > 0 ? (float)listw : 260.0f;
}

//-----------------------------------------------------------------------------

void orbiter::ScenarioTab::SetConfig (Config *cfg)
{
	cfg->CfgLogicPrm.bStartPaused = m_paused;
	cfg->CfgWindowPos.LaunchpadScnListWidth = (int)m_listWidth;
}

//-----------------------------------------------------------------------------

void orbiter::ScenarioTab::Activated ()
{
	RefreshList (true);
}

//-----------------------------------------------------------------------------

void orbiter::ScenarioTab::LaunchpadShowing (bool show)
{
	if (show)
		RefreshList (false);
}

//-----------------------------------------------------------------------------

std::string orbiter::ScenarioTab::SelectedScenario () const
{
	return m_selDir ? std::string () : m_sel;
}

//-----------------------------------------------------------------------------

fs::path orbiter::ScenarioTab::ScnDir () const
{
	std::string dir = Normalise (pCfg->CfgDirPrm.ScnDir);
	if (dir.empty ()) dir = "Scenarios";
	return fs::path (dir);
}

//-----------------------------------------------------------------------------

void orbiter::ScenarioTab::ScanDirectory (const fs::path &path, Node &node)
{
	node.children.clear ();
	std::vector<Node> dirs, files;
	std::error_code ec;
	for (auto &entry : fs::directory_iterator (path, ec)) {
		std::string name = entry.path ().filename ().string ();
		if (name.empty () || name[0] == '.') continue;
		if (entry.is_directory (ec)) {
			Node n{ name, true, {} };
			ScanDirectory (entry.path (), n);
			dirs.push_back (std::move (n));
		}
		else if (entry.is_regular_file (ec) && !strcasecmp (entry.path ().extension ().string ().c_str (), ".scn")) {
			files.push_back ({ entry.path ().stem ().string (), false, {} });
		}
	}
	auto less = [](const Node &a, const Node &b) { return CaseLess (a.name, b.name); };
	std::sort (files.begin (), files.end (), less);
	std::sort (dirs.begin (), dirs.end (), less);
	node.children = std::move (files);
	for (auto &d : dirs) node.children.push_back (std::move (d));
}

//-----------------------------------------------------------------------------

const orbiter::ScenarioTab::Node *orbiter::ScenarioTab::FindNode (const std::string &path) const
{
	const Node *node = &m_root;
	size_t p = 0;
	while (p <= path.size ()) {
		size_t q = path.find ('/', p);
		std::string part = path.substr (p, q == std::string::npos ? std::string::npos : q - p);
		const Node *next = nullptr;
		for (auto &c : node->children)
			if (c.name == part) { next = &c; break; }
		if (!next) return nullptr;
		node = next;
		if (q == std::string::npos) return node;
		p = q + 1;
	}
	return nullptr;
}

//-----------------------------------------------------------------------------

void orbiter::ScenarioTab::RefreshList (bool preserveSelection)
{
	Node root{ "", true, {} };
	ScanDirectory (ScnDir (), root);
	m_lastScan = Now ();
	bool changed = !(root == m_root);
	m_root = std::move (root);

	std::string sel;
	if (preserveSelection && !m_sel.empty ()) {
		sel = m_sel;
		while (!sel.empty () && !FindNode (sel)) {
			size_t p = sel.rfind ('/');
			sel = p == std::string::npos ? std::string () : sel.substr (0, p);
		}
	}
	else if (!preserveSelection) {
		for (auto &c : m_root.children)
			if (!c.dir && c.name == CurrentScenario) { sel = c.name; break; }
		if (sel.empty () && !m_root.children.empty ())
			sel = m_root.children.front ().name;
	}
	if (sel != m_sel || changed || !preserveSelection)
		Select (sel, !preserveSelection || sel != m_sel);
}

//-----------------------------------------------------------------------------

void orbiter::ScenarioTab::Select (const std::string &path, bool reveal)
{
	const Node *node = path.empty () ? nullptr : FindNode (path);
	m_sel = node ? path : std::string ();
	m_selDir = node ? node->dir : false;
	if (reveal) m_reveal = true;
	pLp->EnableLaunchButton (node && !node->dir);
	LoadDescription ();
}

//-----------------------------------------------------------------------------

void orbiter::ScenarioTab::DrawNode (const Node &node, const std::string &path)
{
	bool selected = (path == m_sel);
	ImGuiTreeNodeFlags flags = ImGuiTreeNodeFlags_SpanAvailWidth | ImGuiTreeNodeFlags_NavLeftJumpsToParent;
	if (selected) flags |= ImGuiTreeNodeFlags_Selected;
	if (node.dir) {
		flags |= ImGuiTreeNodeFlags_OpenOnArrow | ImGuiTreeNodeFlags_OpenOnDoubleClick;
		if (m_reveal && m_sel.size () > path.size () && !m_sel.compare (0, path.size (), path) && m_sel[path.size ()] == '/')
			ImGui::SetNextItemOpen (true);
	}
	else
		flags |= ImGuiTreeNodeFlags_Leaf | ImGuiTreeNodeFlags_NoTreePushOnOpen;

	bool open = ImGui::TreeNodeEx (node.name.c_str (), flags);
	if (ImGui::IsItemClicked () && !ImGui::IsItemToggledOpen () && !selected)
		Select (path, false);
	if (!node.dir && ImGui::IsItemHovered () && ImGui::IsMouseDoubleClicked (ImGuiMouseButton_Left))
		pLp->LaunchScenario (path.c_str ());
	if (!node.dir && ImGui::IsItemFocused () && (ImGui::IsKeyPressed (ImGuiKey_Enter) || ImGui::IsKeyPressed (ImGuiKey_KeypadEnter)))
		pLp->LaunchScenario (path.c_str ());
	if (selected && m_reveal) {
		ImGui::SetScrollHereY (0.5f);
		m_reveal = false;
	}

	if (node.dir && open) {
		for (auto &c : node.children)
			DrawNode (c, path + "/" + c.name);
		ImGui::TreePop ();
	}
}

//-----------------------------------------------------------------------------

void orbiter::ScenarioTab::Draw ()
{
	const float sc = pLp->UIScale ();
	const ImGuiStyle &style = ImGui::GetStyle ();

	if (pCfg->CfgDebugPrm.bHtmlScnDesc != m_descMode)
		LoadDescription ();
	if (Now () - m_lastScan > 2.0)
		RefreshList (true);

	float footer = ImGui::GetFrameHeight () + style.ItemSpacing.y;
	float h = ImGui::GetContentRegionAvail ().y - footer;

	ImGui::BeginChild ("##scnlist", ImVec2 (m_listWidth * sc, h), ImGuiChildFlags_Borders | ImGuiChildFlags_ResizeX);
	for (auto &c : m_root.children)
		DrawNode (c, c.name);
	m_listWidth = ImGui::GetWindowWidth () / sc;
	ImGui::EndChild ();

	ImGui::SameLine ();
	ImGui::BeginChild ("##scndesc", ImVec2 (0, h), ImGuiChildFlags_Borders);
	DrawDescription ();
	ImGui::EndChild ();

	if (ImGui::Button ("Save current..."))
		m_openSave = true;
	ImGui::SameLine ();
	if (ImGui::Button ("Clear quicksaves"))
		BeginClearQuicksaves ();

	const char *pausedLabel = "Start paused";
	float cw = ImGui::GetFrameHeight () + style.ItemInnerSpacing.x + ImGui::CalcTextSize (pausedLabel).x;
	ImGui::SameLine (ImGui::GetContentRegionMax ().x - cw);
	ImGui::Checkbox (pausedLabel, &m_paused);

	DrawSaveDialog ();
	DrawClearDialog ();
}

//-----------------------------------------------------------------------------

void orbiter::ScenarioTab::ClearDescription ()
{
	for (auto &b : m_desc)
		if (b.tex) pLp->ReleaseImage (b.tex);
	m_desc.clear ();
}

//-----------------------------------------------------------------------------

void orbiter::ScenarioTab::LoadDescription ()
{
	ClearDescription ();
	m_descMode = pCfg->CfgDebugPrm.bHtmlScnDesc;
	if (m_sel.empty ()) return;

	bool html = (m_descMode != 0);
	fs::path file = m_selDir ? ScnDir () / m_sel / "Description.txt" : ScnDir () / (m_sel + ".scn");
	std::string data, block;
	if (!ReadFile (file, data)) return;
	data = ToUtf8 (data);
	fs::path htmlRoot = FindCI ("Html/Scenarios");

	if (html && ExtractBlock (data, "URLDESC", block)) {
		std::string ref = Normalise (Trim (block));
		std::string path = ref, topic;
		size_t comma = ref.find (',');
		if (comma != std::string::npos) {
			path = Trim (ref.substr (0, comma));
			topic = Trim (ref.substr (comma + 1));
		}
		fs::path page = topic.empty () ? htmlRoot / (path + ".htm") : htmlRoot / path / (topic + ".htm");
		page = FindCI (page);
		std::string content;
		if (ReadFile (page, content)) {
			ParseHtml (ToUtf8 (content), page.parent_path ());
			return;
		}
	}
	if (html) {
		if (ExtractBlock (data, "HYPERDESC", block))
			ParseHtml (block, htmlRoot);
		else if (ExtractBlock (data, "DESC", block))
			ParseText (block);
	}
	else {
		if (ExtractBlock (data, "DESC", block))
			ParseText (block);
		else if (ExtractBlock (data, "HYPERDESC", block)) {
			ParseHtml (block, htmlRoot);
			m_desc.erase (std::remove_if (m_desc.begin (), m_desc.end (), [](const Block &b) { return b.type == Block::IMAGE; }), m_desc.end ());
			for (auto &b : m_desc)
				for (auto &r : b.runs) r.url.clear ();
		}
	}
}

//-----------------------------------------------------------------------------

void orbiter::ScenarioTab::ParseText (const std::string &text)
{
	std::istringstream is (text);
	std::string line, para;
	auto flush = [&]() {
		std::string t = Trim (para);
		if (!t.empty ()) {
			Block b;
			b.type = Block::TEXT;
			b.runs.push_back ({ t, "" });
			m_desc.push_back (std::move (b));
		}
		para.clear ();
	};
	while (std::getline (is, line)) {
		if (!line.empty () && line.back () == '\r') line.pop_back ();
		if (Trim (line).empty ()) flush ();
		else {
			if (!para.empty ()) para += ' ';
			para += Trim (line);
		}
	}
	flush ();
}

//-----------------------------------------------------------------------------

void orbiter::ScenarioTab::ParseHtml (const std::string &html, const fs::path &base)
{
	Block cur;
	cur.type = Block::TEXT;
	std::string url;
	std::string skipTag;
	bool tightNext = false;

	auto flush = [&](Block::Type next) {
		std::vector<Run> runs;
		for (auto &r : cur.runs)
			if (!r.text.empty ()) runs.push_back (r);
		while (!runs.empty ()) {
			size_t a = runs.front ().text.find_first_not_of (' ');
			if (a == std::string::npos) { runs.erase (runs.begin ()); continue; }
			runs.front ().text.erase (0, a);
			break;
		}
		while (!runs.empty ()) {
			size_t b = runs.back ().text.find_last_not_of (' ');
			if (b == std::string::npos) { runs.pop_back (); continue; }
			runs.back ().text.erase (b + 1);
			break;
		}
		if (!runs.empty ()) {
			cur.runs = std::move (runs);
			cur.tight = tightNext;
			m_desc.push_back (std::move (cur));
			tightNext = false;
		}
		cur = Block ();
		cur.type = next;
	};
	auto addText = [&](const std::string &raw) {
		std::string t;
		for (char c : raw) {
			char ch = (c == '\r' || c == '\n' || c == '\t') ? ' ' : c;
			if (ch == ' ') {
				bool lastSpace = !t.empty () ? t.back () == ' '
					: (!cur.runs.empty () && !cur.runs.back ().text.empty () && cur.runs.back ().text.back () == ' ');
				if (lastSpace) continue;
			}
			t += ch;
		}
		t = DecodeEntities (t);
		if (t.empty ()) return;
		if (cur.runs.empty () || cur.runs.back ().url != url)
			cur.runs.push_back ({ t, url });
		else
			cur.runs.back ().text += t;
	};

	size_t i = 0, n = html.size ();
	while (i < n) {
		if (html[i] == '<') {
			if (!html.compare (i, 4, "<!--")) {
				size_t e = html.find ("-->", i + 4);
				i = (e == std::string::npos ? n : e + 3);
				continue;
			}
			size_t e = html.find ('>', i);
			if (e == std::string::npos) break;
			std::string tag = html.substr (i + 1, e - i - 1);
			i = e + 1;
			bool closing = !tag.empty () && tag[0] == '/';
			size_t s = closing ? 1 : 0;
			size_t te = s;
			while (te < tag.size () && !isspace ((unsigned char)tag[te]) && tag[te] != '/' && tag[te] != '>') te++;
			std::string name = ToLower (tag.substr (s, te - s));

			if (!skipTag.empty ()) {
				if (closing && name == skipTag) skipTag.clear ();
				continue;
			}
			if (!closing && (name == "head" || name == "style" || name == "script" || name == "title")) {
				skipTag = name;
				continue;
			}
			bool heading = name.size () == 2 && name[0] == 'h' && name[1] >= '1' && name[1] <= '6';
			if (heading) flush (closing ? Block::TEXT : Block::HEADING);
			else if (name == "li") flush (closing ? Block::TEXT : Block::BULLET);
			else if (name == "br") { flush (cur.type); tightNext = true; }
			else if (name == "p" || name == "div" || name == "ul" || name == "ol" || name == "table" || name == "tr"
				|| name == "hr" || name == "blockquote" || name == "dl" || name == "dt" || name == "dd" || name == "pre")
				flush (Block::TEXT);
			else if (name == "a") {
				url = closing ? std::string () : Attribute (tag, "href");
			}
			else if (name == "img" && !closing) {
				std::string src = Normalise (Attribute (tag, "src"));
				if (!src.empty ()) {
					Block::Type t = cur.type;
					flush (t);
					Block img;
					img.type = Block::IMAGE;
					img.image = FindCI (base / src).string ();
					std::string w = Attribute (tag, "width");
					img.fullWidth = !w.empty () && w.back () == '%';
					m_desc.push_back (std::move (img));
				}
			}
			else if (name == "td" || name == "th") {
				addText (" ");
			}
		}
		else {
			size_t e = html.find ('<', i);
			if (e == std::string::npos) e = n;
			if (skipTag.empty ())
				addText (html.substr (i, e - i));
			i = e;
		}
	}
	flush (Block::TEXT);
	m_descBase = base;
}

//-----------------------------------------------------------------------------

void orbiter::ScenarioTab::DrawRuns (const std::vector<Run> &runs)
{
	float wrap = ImGui::GetContentRegionAvail ().x;
	float space = ImGui::CalcTextSize (" ").x;
	float x = 0.0f;
	int id = 0;
	ImGui::PushStyleVar (ImGuiStyleVar_ItemSpacing, ImVec2 (0.0f, 1.0f));
	ImGui::BeginGroup ();
	for (auto &r : runs) {
		size_t p = 0;
		while (p < r.text.size ()) {
			bool lead = (r.text[p] == ' ');
			size_t a = r.text.find_first_not_of (' ', p);
			if (a == std::string::npos) break;
			size_t b = r.text.find (' ', a);
			if (b == std::string::npos) b = r.text.size ();
			std::string word = r.text.substr (a, b - a);
			float w = ImGui::CalcTextSize (word.c_str ()).x;
			if (x > 0.0f) {
				float gap = lead ? space : 0.0f;
				if (x + gap + w > wrap) x = 0.0f;
				else { ImGui::SameLine (0.0f, gap); x += gap; }
			}
			if (r.url.empty ())
				ImGui::TextUnformatted (word.c_str ());
			else {
				ImGui::PushID (id++);
				if (ImGui::TextLink (word.c_str ())) {
					std::string u = r.url;
					if (u.compare (0, 4, "http") && u.compare (0, 7, "mailto:")) {
						std::error_code ec;
						u = "file://" + fs::absolute (FindCI (m_descBase / Normalise (u)), ec).string ();
					}
					oplat::OpenURL (u);
				}
				if (ImGui::IsItemHovered ())
					ImGui::SetTooltip ("%s", r.url.c_str ());
				ImGui::PopID ();
			}
			x += w;
			p = b;
		}
	}
	ImGui::EndGroup ();
	ImGui::PopStyleVar ();
}

//-----------------------------------------------------------------------------

void orbiter::ScenarioTab::DrawDescription ()
{
	const ImGuiStyle &style = ImGui::GetStyle ();
	bool first = true;
	for (size_t i = 0; i < m_desc.size (); i++) {
		Block &b = m_desc[i];
		if (!first && !b.tight)
			ImGui::Dummy (ImVec2 (0.0f, style.ItemSpacing.y * 0.5f));
		first = false;
		ImGui::PushID ((int)i);
		switch (b.type) {
		case Block::HEADING:
			ImGui::PushFont (pLp->HeadingFont (), pLp->HeadingFont () ? pLp->HeadingFont ()->LegacySize : 0.0f);
			DrawRuns (b.runs);
			ImGui::PopFont ();
			break;
		case Block::TEXT:
			DrawRuns (b.runs);
			break;
		case Block::BULLET:
			ImGui::Bullet ();
			ImGui::SameLine ();
			DrawRuns (b.runs);
			break;
		case Block::IMAGE: {
			if (!b.loaded) {
				b.loaded = true;
				b.tex = pLp->LoadImage (b.image.c_str (), &b.w, &b.h);
			}
			if (b.tex && b.w > 0 && b.h > 0) {
				float avail = ImGui::GetContentRegionAvail ().x;
				float w = (b.fullWidth || b.w > avail) ? avail : (float)b.w;
				ImGui::Image (ImTextureRef (b.tex), ImVec2 (w, w * (float)b.h / (float)b.w));
			}
			} break;
		}
		ImGui::PopID ();
	}
}

//-----------------------------------------------------------------------------

int orbiter::ScenarioTab::SaveCurScenarioAs (const std::string &name, const std::string &desc, bool replace)
{
	fs::path path = ScnDir () / (Normalise (name) + ".scn");
	std::error_code ec;
	if (!replace && fs::exists (path, ec)) return 2;
	std::string cur;
	if (!ReadFile (ScnDir () / (std::string (CurrentScenario) + ".scn"), cur)) return 1;
	if (path.has_parent_path ()) fs::create_directories (path.parent_path (), ec);
	std::ofstream ofs (path);
	if (!ofs) return 1;
	ofs << "BEGIN_DESC" << std::endl;
	ofs << desc << std::endl;
	ofs << "END_DESC" << std::endl;
	std::istringstream is (cur);
	std::string line, skip;
	while (std::getline (is, line)) {
		if (!line.empty () && line.back () == '\r') line.pop_back ();
		if (!skip.empty ()) {
			if (line == skip) skip.clear ();
			continue;
		}
		if (line == "BEGIN_DESC" || line == "BEGIN_HYPERDESC" || line == "BEGIN_URLDESC") {
			skip = "END_" + line.substr (6);
			continue;
		}
		ofs << line << std::endl;
	}
	return ofs.good () ? 0 : 1;
}

//-----------------------------------------------------------------------------

void orbiter::ScenarioTab::DrawSaveDialog ()
{
	if (m_openSave) {
		m_openSave = false;
		std::error_code ec;
		if (fs::exists (ScnDir () / (std::string (CurrentScenario) + ".scn"), ec)) {
			m_saveName[0] = '\0';
			m_saveDesc[0] = '\0';
			m_saveExists = false;
			m_saveError.clear ();
			ImGui::OpenPopup ("Save current exit state");
		}
		else
			ImGui::OpenPopup ("Save error");
	}

	ImGuiViewport *vp = ImGui::GetMainViewport ();
	ImGui::SetNextWindowPos (vp->GetCenter (), ImGuiCond_Appearing, ImVec2 (0.5f, 0.5f));
	if (ImGui::BeginPopupModal ("Save error", nullptr, ImGuiWindowFlags_AlwaysAutoResize | ImGuiWindowFlags_NoSavedSettings)) {
		ImGui::TextUnformatted ("No current simulation state available.");
		if (ImGui::Button ("OK", ImVec2 (ImGui::GetFontSize () * 5.0f, 0)))
			ImGui::CloseCurrentPopup ();
		ImGui::EndPopup ();
	}

	const float em = ImGui::GetFontSize ();
	ImGui::SetNextWindowPos (vp->GetCenter (), ImGuiCond_Appearing, ImVec2 (0.5f, 0.5f));
	ImGui::SetNextWindowSize (ImVec2 (em * 26.0f, 0), ImGuiCond_Appearing);
	if (ImGui::BeginPopupModal ("Save current exit state", nullptr, ImGuiWindowFlags_NoSavedSettings)) {
		if (ImGui::IsWindowAppearing ())
			ImGui::SetKeyboardFocusHere ();
		ImGui::TextUnformatted ("Scenario name");
		ImGui::SetNextItemWidth (-FLT_MIN);
		ImGui::BeginDisabled (m_saveExists);
		ImGui::InputText ("##name", m_saveName, sizeof (m_saveName));
		ImGui::TextUnformatted ("Description");
		ImGui::InputTextMultiline ("##desc", m_saveDesc, sizeof (m_saveDesc), ImVec2 (-FLT_MIN, em * 9.0f));
		ImGui::EndDisabled ();

		if (!m_saveError.empty ()) {
			ImGui::PushStyleColor (ImGuiCol_Text, ImVec4 (1.0f, 0.45f, 0.4f, 1.0f));
			ImGui::TextWrapped ("%s", m_saveError.c_str ());
			ImGui::PopStyleColor ();
		}

		float bw = em * 5.0f;
		auto save = [&](bool replace) {
			std::string name = Trim (m_saveName);
			int res = SaveCurScenarioAs (name, m_saveDesc, replace);
			if (res == 2) {
				m_saveExists = true;
				m_saveError.clear ();
			}
			else if (res == 1) {
				m_saveExists = false;
				m_saveError = "Error writing scenario file.";
			}
			else {
				ImGui::CloseCurrentPopup ();
				RefreshList (true);
				Select (Normalise (name), true);
			}
		};
		if (m_saveExists) {
			ImGui::TextUnformatted ("File exists. Overwrite?");
			if (ImGui::Button ("Yes", ImVec2 (bw, 0)))
				save (true);
			ImGui::SameLine ();
			if (ImGui::Button ("No", ImVec2 (bw, 0)))
				m_saveExists = false;
		}
		else {
			if (ImGui::Button ("OK", ImVec2 (bw, 0))) {
				if (Trim (m_saveName).empty ())
					m_saveError = "Enter a scenario name.";
				else
					save (false);
			}
			ImGui::SameLine ();
			if (ImGui::Button ("Cancel", ImVec2 (bw, 0)) || ImGui::IsKeyPressed (ImGuiKey_Escape))
				ImGui::CloseCurrentPopup ();
		}
		ImGui::EndPopup ();
	}
}

//-----------------------------------------------------------------------------

void orbiter::ScenarioTab::BeginClearQuicksaves ()
{
	m_quicksaves.clear ();
	fs::path qs = ScnDir () / "Quicksave";
	std::error_code ec;
	if (fs::is_directory (qs, ec)) {
		for (auto &entry : fs::directory_iterator (qs, ec))
			if (entry.is_regular_file (ec) && !strcasecmp (entry.path ().extension ().string ().c_str (), ".scn"))
				m_quicksaves.push_back (entry.path ().stem ().string ());
	}
	std::sort (m_quicksaves.begin (), m_quicksaves.end (), CaseLess);
	m_openClear = true;
}

//-----------------------------------------------------------------------------

void orbiter::ScenarioTab::DrawClearDialog ()
{
	if (m_openClear) {
		m_openClear = false;
		ImGui::OpenPopup ("Clear quicksaves");
	}
	ImGuiViewport *vp = ImGui::GetMainViewport ();
	ImGui::SetNextWindowPos (vp->GetCenter (), ImGuiCond_Appearing, ImVec2 (0.5f, 0.5f));
	if (ImGui::BeginPopupModal ("Clear quicksaves", nullptr, ImGuiWindowFlags_AlwaysAutoResize | ImGuiWindowFlags_NoSavedSettings)) {
		float bw = ImGui::GetFontSize () * 5.0f;
		if (m_quicksaves.empty ()) {
			ImGui::TextUnformatted ("There are no quicksaves to delete.");
			if (ImGui::Button ("OK", ImVec2 (bw, 0)))
				ImGui::CloseCurrentPopup ();
		}
		else {
			ImGui::TextUnformatted ("Are you sure you want to delete all quicksaves? This affects:");
			for (size_t i = 0; i < m_quicksaves.size () && i < 10; i++)
				ImGui::BulletText ("%s", m_quicksaves[i].c_str ());
			if (m_quicksaves.size () > 10)
				ImGui::Text ("... and %d more.", (int)m_quicksaves.size () - 10);
			ImGui::Spacing ();
			if (ImGui::Button ("Delete", ImVec2 (bw, 0))) {
				fs::path qs = ScnDir () / "Quicksave";
				std::error_code ec;
				fs::remove_all (qs, ec);
				if (!ec) fs::create_directory (qs, ec);
				ImGui::CloseCurrentPopup ();
				RefreshList (true);
			}
			ImGui::SameLine ();
			if (ImGui::Button ("Cancel", ImVec2 (bw, 0)) || ImGui::IsKeyPressed (ImGuiKey_Escape))
				ImGui::CloseCurrentPopup ();
		}
		ImGui::EndPopup ();
	}
}

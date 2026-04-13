// Copyright (c) Gondos
// Licensed under the MIT License
#define OAPI_IMPLEMENTATION

#include "i18n.h"
#include <vector>
#include <forward_list>
#include <unordered_map>
#include <string>
#include <string_view>
#include <fstream>
#include <sstream>
#include <iostream>
#include <stdexcept>
#include <cctype>
#include <cstring>
#include <regex>
#include <filesystem>
#include <set>

namespace I18N {

// -------------------- Structures --------------------
struct Translation {
    std::vector<std::string> plurals; // plurals[0] = singular, plurals[1..] = plural forms
};

struct PluralRule {
    int nplurals;
    int (*get_index)(unsigned int n);
};

// -------------------- Global state --------------------
static std::pair<std::string,std::string> g_currentLocale;
static std::vector<std::pair<std::string,std::string>> g_locales; // pair<locale, autonym>
static bool g_notifyMissing = true;
static PluralRule g_currentPluralRule = {2, [](unsigned int n){ return n != 1 ? 1 : 0; }};

// -------------------- Translation storage --------------------
static std::unordered_map<uint64_t, Translation> g_translations;
static std::unordered_map<uint64_t, std::string> g_originalEntities;

// -------------------- Helpers --------------------
static std::string RemoveSpaces(const std::string &s) {
    std::string out; out.reserve(s.size());
    for (char c : s) if (!isspace(static_cast<unsigned char>(c))) out += static_cast<char>(tolower(c));
    return out;
}

static void SelectPluralRuleFromHeader(const std::string &header) {
    std::string normalized = RemoveSpaces(header);

    static const std::vector<std::pair<std::string, PluralRule>> knownRules = {
        {"nplurals=2;plural=(n!=1)", {2, [](unsigned int n){ return n != 1 ? 1 : 0; }}}, // English
        {"nplurals=2;plural=(n>1)",  {2, [](unsigned int n){ return n > 1 ? 1 : 0; }}}, // French
		{"nplurals=1;plural=0",      {1, [](unsigned int n){ return 0;}}}, // Japanese
        {"nplurals=3;plural=(n%10==1&&n%100!=11?0:n%10>=2&&n%10<=4&&(n%100<10||n%100>=20)?1:2)",
                                         {3, [](unsigned int n){ return (n%10==1 && n%100!=11) ? 0 :
                                                                       (n%10>=2 && n%10<=4 && (n%100<10 || n%100>=20)) ? 1 : 2; }}}, // Russian
    };

    for (auto &rule : knownRules) {
        if (normalized.find(rule.first) != std::string::npos) {
            g_currentPluralRule = rule.second;
            return;
        }
    }
	// For now, no plurals are used so don't bother asserting and just fallback to singular
    //throw std::runtime_error("Unsupported plural form in PO file: " + header);
	g_currentPluralRule = {1, [](unsigned int n){ return 0;}};
}
inline uint32_t next_codepoint(const char*& p, const char* end)
{
    unsigned char c = static_cast<unsigned char>(*p++);

    if (c < 0x80)
        return c;

    if ((c >> 5) == 0x6 && p < end)
    {
        uint32_t cp = (c & 0x1F) << 6;
        cp |= (static_cast<unsigned char>(*p++) & 0x3F);
        return cp;
    }

    if ((c >> 4) == 0xE && p + 1 < end)
    {
        uint32_t cp = (c & 0x0F) << 12;
        cp |= (static_cast<unsigned char>(*p++) & 0x3F) << 6;
        cp |= (static_cast<unsigned char>(*p++) & 0x3F);
        return cp;
    }

    if ((c >> 3) == 0x1E && p + 2 < end)
    {
        uint32_t cp = (c & 0x07) << 18;
        cp |= (static_cast<unsigned char>(*p++) & 0x3F) << 12;
        cp |= (static_cast<unsigned char>(*p++) & 0x3F) << 6;
        cp |= (static_cast<unsigned char>(*p++) & 0x3F);
        return cp;
    }

    return 0xFFFD; // replacement char on malformed input
}
//-----------------------------------------------------------------------------
// Map accented Latin-1 / Extended-A to base letters
constexpr uint32_t remove_diacritic(uint32_t cp)
{
    switch(cp)
    {
        // Uppercase A
        case 0x00C0: case 0x00C1: case 0x00C2: case 0x00C3: case 0x00C4: case 0x00C5: return 'A';
        // Uppercase E
        case 0x00C8: case 0x00C9: case 0x00CA: case 0x00CB: return 'E';
        // Uppercase I
        case 0x00CC: case 0x00CD: case 0x00CE: case 0x00CF: return 'I';
        // Uppercase O
        case 0x00D2: case 0x00D3: case 0x00D4: case 0x00D5: case 0x00D6: return 'O';
        // Uppercase U
        case 0x00D9: case 0x00DA: case 0x00DB: case 0x00DC: return 'U';
        // Lowercase a
        case 0x00E0: case 0x00E1: case 0x00E2: case 0x00E3: case 0x00E4: case 0x00E5: return 'a';
        // Lowercase e
        case 0x00E8: case 0x00E9: case 0x00EA: case 0x00EB: return 'e';
        // Lowercase i
        case 0x00EC: case 0x00ED: case 0x00EE: case 0x00EF: return 'i';
        // Lowercase o
        case 0x00F2: case 0x00F3: case 0x00F4: case 0x00F5: case 0x00F6: return 'o';
        // Lowercase u
        case 0x00F9: case 0x00FA: case 0x00FB: case 0x00FC: return 'u';
        // Other common letters
        case 0x00E7: return 'c'; // ç
        case 0x00F1: return 'n'; // ñ
        case 0x00C7: return 'C';
        case 0x00D1: return 'N';
        default: return cp;
    }
}
//-----------------------------------------------------------------------------
// Check if a codepoint is a combining mark (diacritic)
constexpr bool is_combining(uint32_t cp)
{
    return (cp >= 0x0300 && cp <= 0x036F)
        || (cp >= 0x1AB0 && cp <= 0x1AFF)
        || (cp >= 0x1DC0 && cp <= 0x1DFF)
        || (cp >= 0x20D0 && cp <= 0x20FF)
        || (cp >= 0xFE20 && cp <= 0xFE2F);
}

//-----------------------------------------------------------------------------
// Append a codepoint as UTF-8 to std::string
inline void append_utf8(std::string& out, uint32_t cp)
{
    if (cp < 0x80) {
        out.push_back(static_cast<char>(cp));
    } else if (cp < 0x800) {
        out.push_back(static_cast<char>(0xC0 | (cp >> 6)));
        out.push_back(static_cast<char>(0x80 | (cp & 0x3F)));
    } else if (cp < 0x10000) {
        out.push_back(static_cast<char>(0xE0 | (cp >> 12)));
        out.push_back(static_cast<char>(0x80 | ((cp >> 6) & 0x3F)));
        out.push_back(static_cast<char>(0x80 | (cp & 0x3F)));
    } else {
        out.push_back(static_cast<char>(0xF0 | (cp >> 18)));
        out.push_back(static_cast<char>(0x80 | ((cp >> 12) & 0x3F)));
        out.push_back(static_cast<char>(0x80 | ((cp >> 6) & 0x3F)));
        out.push_back(static_cast<char>(0x80 | (cp & 0x3F)));
    }
}

//-----------------------------------------------------------------------------
// Folded append (diacritics removed, case folded, ß → ss, Cyrillic/Greek still works)
inline void append_folded(std::string& out, uint32_t cp)
{
    // First remove diacritic
    cp = remove_diacritic(cp);

    // Skip combining marks (should be mostly gone now)
    if ((cp >= 0x0300 && cp <= 0x036F)) return;

    // ASCII uppercase → lowercase
    if (cp >= 'A' && cp <= 'Z') { append_utf8(out, cp + 32); return; }

    // ß → ss
    if (cp == 0x00DF) { append_utf8(out, 's'); append_utf8(out, 's'); return; }

    // Latin Extended-A / B: still fold uppercase to lowercase
    if ((cp >= 0x0100 && cp <= 0x017F) || (cp >= 0x0180 && cp <= 0x024F))
    {
        if (cp % 2 == 0) cp += 1; // even → lowercase
        append_utf8(out, cp);
        return;
    }

    // Cyrillic uppercase → lowercase
    if (cp >= 0x0410 && cp <= 0x042F) { append_utf8(out, cp + 32); return; }
    if (cp == 0x0401) { append_utf8(out, 0x0451); return; }

    // Greek uppercase + sigma
    if (cp == 0x03A3 || cp == 0x03C2) { append_utf8(out, 0x03C3); return; }
    if ((cp >= 0x0391 && cp <= 0x03A1) || (cp >= 0x03A4 && cp <= 0x03AB)) { append_utf8(out, cp + 32); return; }

    // Default: append as-is
    append_utf8(out, cp);
}

    inline std::string unicode_fold(std::string_view input)
    {
        std::string out;
        out.reserve(input.size()); // worst case: same size

        const char* p = input.data();
        const char* end = p + input.size();

        while (p < end)
        {
            uint32_t cp = next_codepoint(p, end);
            append_folded(out, cp);
        }

        return out;
    }
// -------------------- PO string unescape --------------------
static char HexDigitToValue(char c) {
    if (c >= '0' && c <= '9') return c - '0';
    if (c >= 'a' && c <= 'f') return 10 + (c - 'a');
    if (c >= 'A' && c <= 'F') return 10 + (c - 'A');
    throw std::runtime_error(std::string("Invalid hex digit: ") + c);
}

static unsigned int ParseHex(const std::string &s, size_t &i, int digits) {
    unsigned int val = 0;
    for (int d = 0; d < digits && i < s.size(); ++d, ++i) val = (val << 4) | HexDigitToValue(s[i]);
    return val;
}

static unsigned int ParseOctal(const std::string &s, size_t &i, int maxDigits = 3) {
    unsigned int val = 0;
    int count = 0;
    while (count < maxDigits && i < s.size() && s[i] >= '0' && s[i] <= '7') {
        val = (val << 3) | (s[i] - '0');
        ++i; ++count;
    }
    return val;
}

static void LoadPO(const char *filename);

static std::string UnescapePOString(const std::string &s) {
    std::string out;
    out.reserve(s.size());
    size_t i = 0;
    if (!s.empty() && s.front() == '"' && s.back() == '"') i = 1;
    size_t end = (!s.empty() && s.back() == '"') ? s.size() - 1 : s.size();

    while (i < end) {
        char c = s[i];
        if (c == '\\' && i + 1 < end) {
            ++i;
            char next = s[i];
            switch (next) {
                case 'n': { out += '\n'; ++i; break; }
                case 'r': { out += '\r'; ++i; break; }
                case 't': { out += '\t'; ++i; break; }
                case '\\': {
					if(i<end - 1 && s[i+1] == 'x') {
						i+=2;
						unsigned int val = ParseHex(s, i, 2);
						out += static_cast<char>(val);
						break;
					} else {
						out += '\\'; ++i; break; 
					}
				}
                case '"': { out += '"'; ++i; break; }
                case 'u': {
                    ++i;
                    unsigned int val = ParseHex(s, i, 4);
                    if (val <= 0x7F) out += static_cast<char>(val);
                    else if (val <= 0x7FF) {
                        out += static_cast<char>(0xC0 | ((val >> 6) & 0x1F));
                        out += static_cast<char>(0x80 | (val & 0x3F));
                    } else {
                        out += static_cast<char>(0xE0 | ((val >> 12) & 0x0F));
                        out += static_cast<char>(0x80 | ((val >> 6) & 0x3F));
                        out += static_cast<char>(0x80 | (val & 0x3F));
                    }
                    break;
                }
                case 'U': {
                    ++i;
                    unsigned int val = ParseHex(s, i, 8);
                    if (val <= 0x7F) out += static_cast<char>(val);
                    else if (val <= 0x7FF) {
                        out += static_cast<char>(0xC0 | ((val >> 6) & 0x1F));
                        out += static_cast<char>(0x80 | (val & 0x3F));
                    } else if (val <= 0xFFFF) {
                        out += static_cast<char>(0xE0 | ((val >> 12) & 0x0F));
                        out += static_cast<char>(0x80 | ((val >> 6) & 0x3F));
                        out += static_cast<char>(0x80 | (val & 0x3F));
                    } else {
                        out += static_cast<char>(0xF0 | ((val >> 18) & 0x07));
                        out += static_cast<char>(0x80 | ((val >> 12) & 0x3F));
                        out += static_cast<char>(0x80 | ((val >> 6) & 0x3F));
                        out += static_cast<char>(0x80 | (val & 0x3F));
                    }
                    break;
                }
                default: { // octal or unknown escape
                    if (next >= '0' && next <= '7') {
                        unsigned int val = ParseOctal(s, i);
                        out += static_cast<char>(val);
                    } else {
                        out += next;
                        ++i;
                    }
                    break;
                }
            }
        } else {
            out += c;
            ++i;
        }
    }
    return out;
}

bool ends_with(const std::string& s, const std::string& suffix)
{
    if (suffix.size() > s.size())
        return false;

    return s.compare(
        s.size() - suffix.size(),
        suffix.size(),
        suffix
    ) == 0;
}

bool starts_with(const std::string& s, const std::string& prefix)
{
    return s.size() >= prefix.size() &&
           s.compare(0, prefix.size(), prefix) == 0;
}

std::string extract_po_string(const std::string& line)
{
    auto first = line.find('"');
    if (first == std::string::npos)
        return {};

    auto last = line.rfind('"');
    if (last <= first)
        return {};

    return UnescapePOString(
        line.substr(first + 1, last - first - 1)
    );
}


void LoadLocale(const char *locale)
{
    auto it = std::find_if(g_locales.begin(), g_locales.end(),
        [locale](const std::pair<std::string, std::string>& p) {
            return p.first == locale;
        });

	if (it == g_locales.end()) {
		g_currentLocale.first.clear();
		g_currentLocale.second.clear();
		return;
	}
    g_currentLocale = *it;
    g_translations.clear();
	g_originalEntities.clear();

	if(g_currentLocale.first.empty()) return;

	std::string suffix = std::string(".") + locale + ".po";
	for (const auto& entry : std::filesystem::directory_iterator("i18n"))
    {
        if (!entry.is_regular_file())
            continue;

        const std::string name = entry.path().filename().string();
		if(ends_with(name, suffix)) {
			LoadPO(entry.path().string().c_str());
		}
    }
}

const char *GetLocaleName(const char *name)
{
    auto it = std::find_if(g_locales.begin(), g_locales.end(),
        [name](const std::pair<std::string, std::string>& p) {
            return p.first == name;
        });
	if(it != g_locales.end()) {
		return it->second.c_str();
	} else {
		return name;
	}
}

void ImportDirectory(const char *dirname)
{
	if(g_currentLocale.first.empty()) return;

	std::string suffix = std::string(".") + g_currentLocale.first + ".po";
	for (const auto& entry : std::filesystem::directory_iterator(dirname))
    {
        if (!entry.is_regular_file())
            continue;

        const std::string name = entry.path().filename().string();
		if(ends_with(name, suffix)) {
			LoadPO(entry.path().string().c_str());
		}
    }
}

// Get the list of locales in the i18n directory,
// matching *.locale.po files (e.g. Orbiter.fr_FR.po)
// Use a set to remove duplicates
//#include <tuple>
static std::set<std::pair<std::string, std::string>> ExtractLocales()
{
    std::set<std::pair<std::string, std::string>> locales;

    for (const auto& entry : std::filesystem::directory_iterator("i18n"))
    {
        if (!entry.is_regular_file())
            continue;

        const auto& path = entry.path();

        if (path.extension() != ".po")
            continue;

        std::string name = path.filename().string();

        size_t po_pos = name.rfind(".po");
        size_t dot_pos = name.rfind('.', po_pos - 1);
        if (po_pos == std::string::npos || dot_pos == std::string::npos)
            continue;

        std::string locale = name.substr(dot_pos + 1, po_pos - dot_pos - 1);

        std::ifstream f(path);
        if (!f.is_open())
            continue;

        std::string line;
        std::string msgctxt, msgid, msgstr;

        while (std::getline(f, line))
        {
            if (starts_with(line, "msgctxt"))
                msgctxt = extract_po_string(line);
            else if (starts_with(line, "msgid"))
                msgid = extract_po_string(line);
            else if (starts_with(line, "msgstr"))
            {
                msgstr = extract_po_string(line);

                if (msgctxt == "locale" && msgid == "LANGUAGE_NAME")
                {
                    locales.emplace(locale, msgstr);
                    break;
                }
            }
        }
    }

    return locales;
}
// -------------------- API --------------------
void Init(bool notifymissing) {
    g_notifyMissing = notifymissing;
    g_locales.clear();

	// en_GB is always available (hardcoded msgids)
    g_locales.emplace_back("en_GB", "English (United Kingdom)");

	auto locales = ExtractLocales();
	for(const auto &locale: locales) {
	    g_locales.emplace_back(locale);
	}
}

const std::vector<std::pair<std::string,std::string>> &GetLocales() { return g_locales; }

DLLEXPORT const char *GetOriginalName(const char *name) {
	std::string folded = unicode_fold(name);
	uint64_t key = I18NKey("Name", folded.c_str());
	auto it = g_originalEntities.find(key);
	if(it == g_originalEntities.end()) return name;
	return it->second.c_str();
}

static void AddKey(const std::string &msgctxt, const std::string &msgid, const Translation &translation) {
	if(msgctxt == "Name") {
		std::string folded =  unicode_fold(translation.plurals[0]);
		g_originalEntities[I18NKey("Name", folded.c_str())] = msgid;
	}

	uint64_t key = I18NKey(msgctxt.c_str(), msgid.c_str());
	auto it = g_translations.find(key);
	if(it != g_translations.end() && it->second.plurals != translation.plurals) {
        std::cerr << "Warning: duplicate translation key with different translation: "
                  << (msgctxt.empty() ? "" : (msgctxt + ":")) << msgid << "\n";
	}
	g_translations[key] = translation;
}

static void LoadPO(const char *filename) {
    std::ifstream f(filename);
    if (!f.is_open()) { if (g_notifyMissing) std::cerr << "Locale file not found: " << filename << "\n"; return; }

    // --- Parse header ---
    std::string line, header;
    bool inHeader = false;
    while (std::getline(f, line)) {
        if (line.substr(0,6)=="msgstr"){ inHeader=true; header+=line.substr(6); }
        else if (inHeader && !line.empty() && line.front()=='"') header+=line;
        else if (inHeader) break;
    }
    if (!header.empty()) SelectPluralRuleFromHeader(header);

    f.clear(); f.seekg(0);

    std::string raw_msgctxt, raw_msgid, raw_msgid_plural;
    Translation currentTranslation;
    std::string *currentTarget = nullptr;

    while (std::getline(f, line)) {
        if (line.empty() || line[0]=='#') { // end entry
            if (!raw_msgid.empty()) {
				AddKey(raw_msgctxt, raw_msgid, currentTranslation);
                raw_msgctxt.clear(); raw_msgid.clear(); raw_msgid_plural.clear();
                currentTranslation.plurals.clear(); currentTarget=nullptr;
            }
            continue;
        }

        if (starts_with(line,"msgctxt")) raw_msgctxt=extract_po_string(line), currentTarget=nullptr;
        else if (starts_with(line,"msgid_plural")) raw_msgid_plural=extract_po_string(line), currentTarget=nullptr;
        else if (starts_with(line,"msgid")) raw_msgid=extract_po_string(line), currentTarget=&raw_msgid;
        else if (starts_with(line,"msgstr ")) { currentTranslation.plurals.resize(1); currentTranslation.plurals[0]=extract_po_string(line); currentTarget=&currentTranslation.plurals[0]; }
        else if (starts_with(line,"msgstr[")) {
            size_t idx_end=line.find(']',6);
            if (idx_end!=std::string::npos) {
                int idx=std::stoi(line.substr(6,idx_end-6));
                if ((size_t)idx>=currentTranslation.plurals.size()) currentTranslation.plurals.resize(idx+1);
                currentTranslation.plurals[idx]=extract_po_string(line);
                currentTarget=&currentTranslation.plurals[idx];
            }
        }
        else if (!line.empty() && line.front()=='"' && currentTarget) *currentTarget += extract_po_string(line);
    }

    if (!raw_msgid.empty()) { // last entry
		AddKey(raw_msgctxt, raw_msgid, currentTranslation);
    }
}

// -------------------- GetText API --------------------

DLLEXPORT bool Enabled()
{
	return !g_currentLocale.first.empty();
}

DLLEXPORT const char *PGetText(uint64_t key) {
    auto it=g_translations.find(key);
    if(it!=g_translations.end()&&!it->second.plurals.empty()&&!it->second.plurals[0].empty()) return it->second.plurals[0].c_str();
    //if(g_notifyMissing) std::cerr << "Missing translation: " << msgid << "\n";
    return NULL;
}

DLLEXPORT const char *PNGetText(uint64_t key,const char *msgid_plural,unsigned int n) {
    auto it=g_translations.find(key);
    if(it!=g_translations.end()&&!it->second.plurals.empty()) {
        int idx=g_currentPluralRule.get_index(n);
        if((size_t)idx>=it->second.plurals.size()) idx=it->second.plurals.size()-1;
        return it->second.plurals[idx].c_str();
    }
    if(g_notifyMissing) std::cerr << "Missing plural translation: " << key << "\n";
    return (n==1)?NULL:msgid_plural;
}

DLLEXPORT std::size_t u8bytes(const char *utf8, std::size_t max_codepoints) noexcept
{
    std::size_t bytes = 0;
    std::size_t count = 0;

    while (utf8[bytes] && count < max_codepoints) {
        const unsigned char c =
            static_cast<unsigned char>(utf8[bytes]);

        std::size_t advance =
            (c & 0x80) == 0x00 ? 1 :
            (c & 0xE0) == 0xC0 ? 2 :
            (c & 0xF0) == 0xE0 ? 3 :
            (c & 0xF8) == 0xF0 ? 4 :
            0;

        if (advance == 0) break; // invalid UTF-8 start byte

        bytes += advance;
        ++count;
    }

    return bytes;
}

DLLEXPORT std::size_t u8len(const char *utf8) noexcept
{
	std::size_t bytes = 0;
    std::size_t count = 0;

    while (utf8[bytes]) {
        const unsigned char c =
            static_cast<unsigned char>(utf8[bytes]);

        std::size_t advance =
            (c & 0x80) == 0x00 ? 1 :
            (c & 0xE0) == 0xC0 ? 2 :
            (c & 0xF0) == 0xE0 ? 3 :
            (c & 0xF8) == 0xF0 ? 4 :
            0;

        if (advance == 0) break; // invalid UTF-8 start byte

        bytes += advance;
        ++count;
    }

    return count;
}

DLLEXPORT std::size_t u8ncpy(char *u8dst, const char *u8src, std::size_t codepoints, std::size_t dest_size) noexcept
{
	if (dest_size == 0) return 0;

    std::size_t bytes_to_copy = u8bytes(u8src, codepoints);

    // truncate if necessary to fit in dest_size-1
	// it may break a codepoint
    if (bytes_to_copy >= dest_size)
        bytes_to_copy = dest_size - 1;

    std::memcpy(u8dst, u8src, bytes_to_copy);
    u8dst[bytes_to_copy] = '\0';

    return bytes_to_copy; // returns number of bytes copied
}


} // namespace I18N

#pragma once
#include <vector>
#include <string>


constexpr uint64_t fnv1a(const char* s, uint64_t h = 14695981039346656037ull)
{
    return *s ? fnv1a(s + 1, (h ^ uint64_t(*s)) * 1099511628211ull) : h;
}

constexpr uint64_t I18NKey(const char* ctx, const char* id)
{
    return fnv1a(id, fnv1a("\4", fnv1a(ctx)));
}

static inline const char *i18nfallback(const char *value, const char *defvalue) { return value?value:defvalue; }

#ifdef TRANSLATION_CONTEXT
#define _(msgid) i18nfallback(I18N::PGetText(I18NKey(TRANSLATION_CONTEXT,msgid)), msgid)
#else
#define _(msgid) i18nfallback(I18N::PGetText(I18NKey("",msgid)), msgid)
#endif
#define _c(msgctx,msgid) i18nfallback(I18N::PGetText(I18NKey(msgctx,msgid)), msgid)
#define _n(msgid,msgid_plur,N) i18nfallback(I18N::PNGetText(I18NKey("",msgid),msgid_plur,N), msgid)
#define _nc(msgctx,msgid,msgid_plur,N) i18nfallback(I18N::PNGetText(I18NKey(msgctx,msgid),msgid_plur,N), msgid)

// Helper for planet/moon/base names translation
#define _name(msgid) i18nfallback(I18N::PGetText(I18NKey("Name",msgid)), msgid)
#define _revname(name) I18N::GetOriginalName(name)

#define _card(msgid) i18nfallback(I18N::PGetText(I18NKey("Cardinal direction",msgid)), msgid)

// Helper for abbreviations
// /!\ Must match the context used in xgettext.lua
#define _abbr2(msgid) i18nfallback(I18N::PGetText(I18NKey("Abbreviation - 2 letters",msgid)), msgid)
#define _abbr3(msgid) i18nfallback(I18N::PGetText(I18NKey("Abbreviation - 3 letters",msgid)), msgid)
#define _abbr4(msgid) i18nfallback(I18N::PGetText(I18NKey("Abbreviation - 4 letters",msgid)), msgid)


namespace I18N {

// Initialize i18n subsystem
void Init(bool notifymissing);

// Get a list of available locales
const std::vector<std::string> &GetLocales();

// Load a locale
void LoadLocale(const char *);

// /!\ Returned pointer remains valid until the next call to LoadLocale()
// Translate generic text in singular with context
const char *PGetText(uint64_t key);

// Translate generic text, possibly plural with context
const char *PNGetText(uint64_t key, const char *msgid_plural, unsigned int n);

// "Reverse" translate an entity to English (mainly for target selection)
const char *GetOriginalName(const char *name);

// Count the number of bytes taken by the first n codepoints
std::size_t u8bytes(const char *utf8, std::size_t codepoints) noexcept;
// Number of codepoints in the UTF-8 string
std::size_t u8len(const char *utf8) noexcept;
// Copy up to n-codepoints from u8src to u8dst
std::size_t u8ncpy(char *u8dst, const char *u8src, std::size_t codepoints, std::size_t dest_size) noexcept;
}; // namespace

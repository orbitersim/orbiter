#include "M9Internal.h"
#include <fstream>
#include <sstream>
#include <regex>
#include <set>
#include <algorithm>

namespace m9 {

// ---------------------------------------------------------------------------
// Source loading

static std::string DirName(const std::string &p)
{
	size_t s = p.find_last_of("/\\");
	return s == std::string::npos ? std::string(".") : p.substr(0, s);
}

static bool ReadFile(const std::string &path, std::string &out)
{
	std::ifstream f(path, std::ios::binary);
	if (!f) return false;
	std::stringstream ss;
	ss << f.rdbuf();
	out = ss.str();
	return true;
}

static bool Preprocess(const std::string &path, std::string &out, std::set<std::string> &seen, int depth)
{
	if (depth > 16) return false;
	std::string text;
	if (!ReadFile(path, text)) {
		Log("shader source not found: %s", path.c_str());
		return false;
	}
	std::istringstream in(text);
	std::string line;
	std::regex inc("^\\s*#\\s*include\\s*\"([^\"]+)\"");
	while (std::getline(in, line)) {
		std::smatch m;
		if (std::regex_search(line, m, inc)) {
			std::string file = DirName(path) + "/" + m[1].str();
			if (seen.insert(file).second)
				if (!Preprocess(file, out, seen, depth + 1)) return false;
			out += "\n";
			continue;
		}
		out += line;
		out += "\n";
	}
	return true;
}

static std::string StripComments(const std::string &s)
{
	std::string o;
	o.reserve(s.size());
	for (size_t i = 0; i < s.size();) {
		if (s.compare(i, 2, "//") == 0) {
			while (i < s.size() && s[i] != '\n') i++;
		} else if (s.compare(i, 2, "/*") == 0) {
			size_t e = s.find("*/", i + 2);
			i = e == std::string::npos ? s.size() : e + 2;
		} else {
			o += s[i++];
		}
	}
	return o;
}

struct TypeInfo {
	UINT size, align, packed, rows, cols;
	ParamType type;
	bool matrix;
};

static bool BasicType(const std::string &t, TypeInfo &ti)
{
	static const std::regex vec("^(float|int|uint|half|bool|short|ushort)([234])?$");
	static const std::regex mat("^(float|half)([234])x([234])$");
	static const std::regex packed("^packed_(float|int|uint)([234])$");
	std::smatch m;
	if (std::regex_match(t, m, vec)) {
		std::string base = m[1].str();
		UINT n = m[2].matched ? (UINT)std::stoi(m[2].str()) : 1;
		UINT comp = (base == "half" || base == "short" || base == "ushort") ? 2 : base == "bool" ? 1 : 4;
		ti.type = base == "float" || base == "half" ? PT_FLOAT : base == "bool" ? PT_BOOL : PT_INT;
		ti.rows = 1;
		ti.cols = n;
		ti.matrix = false;
		UINT al = comp * (n == 3 ? 4 : n);
		ti.align = al;
		ti.size = n == 3 ? comp * 4 : comp * n;
		ti.packed = 4 * n;
		return true;
	}
	if (std::regex_match(t, m, mat)) {
		UINT c = (UINT)std::stoi(m[2].str()), r = (UINT)std::stoi(m[3].str());
		UINT comp = m[1].str() == "half" ? 2 : 4;
		UINT colSize = r == 3 ? comp * 4 : comp * r;
		ti.type = PT_FLOAT;
		ti.rows = c;
		ti.cols = r;
		ti.matrix = true;
		ti.align = colSize;
		ti.size = colSize * c;
		ti.packed = 4 * r * c;
		return true;
	}
	if (std::regex_match(t, m, packed)) {
		UINT n = (UINT)std::stoi(m[2].str());
		ti.type = m[1].str() == "float" ? PT_FLOAT : PT_INT;
		ti.rows = 1;
		ti.cols = n;
		ti.matrix = false;
		ti.align = 4;
		ti.size = 4 * n;
		ti.packed = 4 * n;
		return true;
	}
	return false;
}

static void ParseStructs(const std::string &src, std::map<std::string, Layout> &out, const D3DXMACRO *macros)
{
	static const std::regex sr("struct\\s+(\\w+)\\s*\\{([^}]*)\\}");
	static const std::regex mr("^\\s*(?:constant\\s+)?([A-Za-z_][\\w]*)\\s+([A-Za-z_]\\w*)\\s*(?:\\[\\s*(\\w+)\\s*\\])?\\s*$");
	std::string text = StripComments(src);
	std::map<std::string, int> defines;
	{
		static const std::regex dr("#\\s*define\\s+(\\w+)\\s+(\\d+)");
		for (auto it = std::sregex_iterator(text.begin(), text.end(), dr); it != std::sregex_iterator(); ++it)
			defines[(*it)[1].str()] = std::stoi((*it)[2].str());
		for (const D3DXMACRO *m = macros; m && m->Name; m++) {
			if (m->Definition && isdigit((unsigned char)m->Definition[0])) defines[m->Name] = atoi(m->Definition);
		}
	}
	for (auto it = std::sregex_iterator(text.begin(), text.end(), sr); it != std::sregex_iterator(); ++it) {
		Layout L;
		L.structName = (*it)[1].str();
		std::string body = (*it)[2].str();
		bool ok = true;
		UINT offset = 0, maxAlign = 4, packed = 0;
		std::stringstream ss(body);
		std::string decl;
		while (std::getline(ss, decl, ';')) {
			if (decl.find('[') != std::string::npos && decl.find("[[") != std::string::npos) {
				ok = false;
				break;
			}
			size_t b = decl.find_first_not_of(" \t\r\n");
			if (b == std::string::npos) continue;
			std::smatch m;
			std::string d = decl.substr(b);
			while (!d.empty() && isspace((unsigned char)d.back())) d.pop_back();
			if (!std::regex_match(d, m, mr)) continue;
			std::string type = m[1].str(), name = m[2].str();
			UINT n = 0;
			if (m[3].matched) {
				std::string cnt = m[3].str();
				if (isdigit((unsigned char)cnt[0])) n = (UINT)std::stoi(cnt);
				else if (defines.count(cnt)) n = (UINT)defines[cnt];
				else {
					Log("struct %s: unresolved array size %s", L.structName.c_str(), cnt.c_str());
					ok = false;
					break;
				}
			}
			Member mb;
			mb.name = name;
			TypeInfo ti;
			if (BasicType(type, ti)) {
				mb.rows = ti.rows;
				mb.cols = ti.cols;
				mb.type = ti.type;
				mb.matrix = ti.matrix;
				mb.packed = ti.packed;
			} else if (out.count(type)) {
				const Layout &sub = out[type];
				ti.size = sub.size;
				ti.align = sub.align;
				ti.packed = sub.packed;
				mb.type = PT_STRUCT;
				mb.sub = std::make_shared<Layout>(sub);
				mb.packed = sub.packed;
			} else {
				ok = false;
				break;
			}
			offset = (offset + ti.align - 1) & ~(ti.align - 1);
			mb.offset = offset;
			mb.elements = n;
			mb.stride = (ti.size + ti.align - 1) & ~(ti.align - 1);
			mb.size = n ? mb.stride * n : ti.size;
			offset += mb.size;
			maxAlign = std::max(maxAlign, ti.align);
			packed += ti.packed * (n ? n : 1);
			L.members.push_back(mb);
		}
		if (!ok || L.members.empty()) continue;
		L.align = maxAlign;
		L.size = (offset + maxAlign - 1) & ~(maxAlign - 1);
		L.packed = packed;
		out[L.structName] = L;
	}
}

const Member *Layout::Find(const char *name) const
{
	if (!name) return nullptr;
	for (const auto &m : members)
		if (m.name == name) return &m;
	return nullptr;
}

static bool IsIdent(char c) { return isalnum((unsigned char)c) || c == '_'; }

static size_t SkipSpaceBack(const std::string &t, size_t i)
{
	while (i > 0 && isspace((unsigned char)t[i - 1])) i--;
	return i;
}

static size_t SkipSpace(const std::string &t, size_t i)
{
	while (i < t.size() && isspace((unsigned char)t[i])) i++;
	return i;
}

static void ScanTextures(const std::string &t, ShaderSource &src)
{
	for (size_t p = t.find("[["); p != std::string::npos; p = t.find("[[", p + 2)) {
		size_t q = SkipSpace(t, p + 2);
		if (t.compare(q, 8, "texture(") != 0) continue;
		q = SkipSpace(t, q + 8);
		size_t d = q;
		while (q < t.size() && isdigit((unsigned char)t[q])) q++;
		if (q == d) continue;
		int slot = std::stoi(t.substr(d, q - d));
		size_t e = SkipSpaceBack(t, p), b = e;
		while (b > 0 && IsIdent(t[b - 1])) b--;
		if (b == e) continue;
		std::string name = t.substr(b, e - b);
		size_t g = SkipSpaceBack(t, b);
		if (g == 0 || t[g - 1] != '>') continue;
		size_t lt = t.rfind('<', g - 1);
		if (lt == std::string::npos) continue;
		size_t ke = SkipSpaceBack(t, lt), kb = ke;
		while (kb > 0 && IsIdent(t[kb - 1])) kb--;
		std::string kind = t.substr(kb, ke - kb);
		if (kind != "texture2d" && kind != "texture3d" && kind != "texturecube" && kind != "depth2d" &&
			kind != "texture2d_array" && kind != "texture2d_ms" && kind != "depthcube") continue;
		src.textureSlots[name] = slot;
		src.textureTypes[name] = kind == "texture3d" ? 3 : (kind == "texturecube" || kind == "depthcube") ? 6 : 2;
	}
}

static void ScanFunctions(const std::string &t, ShaderSource &src)
{
	static const std::regex cr("constant\\s+(\\w+)\\s*&\\s*\\w+\\s*\\[\\[\\s*buffer\\(\\s*0\\s*\\)\\s*\\]\\]");
	for (const char *kw : {"vertex", "fragment"}) {
		size_t n = strlen(kw);
		for (size_t p = t.find(kw); p != std::string::npos; p = t.find(kw, p + n)) {
			if ((p > 0 && IsIdent(t[p - 1])) || p + n >= t.size() || !isspace((unsigned char)t[p + n])) continue;
			size_t q = SkipSpace(t, p + n);
			while (q < t.size() && (IsIdent(t[q]) || t[q] == ':' || t[q] == '<' || t[q] == '>')) q++;
			size_t nb = SkipSpace(t, q), ne = nb;
			while (ne < t.size() && IsIdent(t[ne])) ne++;
			if (ne == nb) continue;
			size_t op = SkipSpace(t, ne);
			if (op >= t.size() || t[op] != '(') continue;
			size_t br = t.find('{', op);
			if (br == std::string::npos) continue;
			size_t cp = SkipSpaceBack(t, br);
			if (cp == 0 || t[cp - 1] != ')') continue;
			std::string params = t.substr(op + 1, cp - 1 - (op + 1));
			std::smatch m;
			if (std::regex_search(params, m, cr)) src.funcConst[t.substr(nb, ne - nb)] = m[1].str();
		}
	}
}

bool LoadShaderSource(const std::string &pathIn, ShaderSource &src, const D3DXMACRO *defines)
{
	std::string path = pathIn;
	for (char &c : path) if (c == '\\') c = '/';
	size_t dot = path.find_last_of('.');
	size_t slash = path.find_last_of('/');
	if (dot != std::string::npos && (slash == std::string::npos || dot > slash)) path = path.substr(0, dot);
	path += ".metal";
	std::string key = path;
	for (const D3DXMACRO *d = defines; d && d->Name; d++) key += std::string("|") + d->Name + "=" + (d->Definition ? d->Definition : "1");
	static std::map<std::string, ShaderSource> cache;
	auto hit = cache.find(key);
	if (hit != cache.end()) {
		src = hit->second;
		return true;
	}
	src = ShaderSource();
	src.path = path;
	std::set<std::string> seen;
	seen.insert(path);
	if (!Preprocess(path, src.text, seen, 0)) return false;
	ParseStructs(src.text, src.structs, defines);
	std::string text = StripComments(src.text);
	ScanTextures(text, src);
	ScanFunctions(text, src);
	cache[key] = src;
	return true;
}

static std::map<std::string, id<MTLLibrary>> g_libraries;

id<MTLLibrary> CompileLibrary(id<MTLDevice> dev, const ShaderSource &src, const D3DXMACRO *defines, std::string &errors)
{
	std::string key = src.path;
	NSMutableDictionary *macros = [NSMutableDictionary dictionary];
	for (const D3DXMACRO *d = defines; d && d->Name; d++) {
		NSString *n = [NSString stringWithUTF8String:d->Name];
		NSString *v = d->Definition ? [NSString stringWithUTF8String:d->Definition] : @"1";
		NSScanner *sc = [NSScanner scannerWithString:v];
		int iv;
		if ([sc scanInt:&iv] && sc.isAtEnd) macros[n] = @(iv);
		else macros[n] = v;
		key += std::string("|") + d->Name + "=" + (d->Definition ? d->Definition : "1");
	}
	auto it = g_libraries.find(key);
	if (it != g_libraries.end()) return it->second;
	MTLCompileOptions *opt = [MTLCompileOptions new];
	opt.preprocessorMacros = macros;
	opt.languageVersion = MTLLanguageVersion3_0;
	opt.mathMode = MTLMathModeFast;
	NSError *err = nil;
	id<MTLLibrary> lib = [dev newLibraryWithSource:[NSString stringWithUTF8String:src.text.c_str()] options:opt error:&err];
	if (!lib) {
		errors = err.localizedDescription.UTF8String;
		Log("shader compile failed (%s):\n%s", src.path.c_str(), errors.c_str());
		return nil;
	}
	g_libraries[key] = lib;
	return lib;
}

// ---------------------------------------------------------------------------
// Parameter writing

static void WriteScalarArray(uint8_t *dst, ParamType dstType, const void *src, ParamType srcType, UINT n)
{
	for (UINT i = 0; i < n; i++) {
		float f = 0;
		int v = 0;
		if (srcType == PT_FLOAT) { f = ((const float *)src)[i]; v = (int)f; }
		else { v = ((const int *)src)[i]; f = (float)v; }
		if (dstType == PT_FLOAT) memcpy(dst + i * 4, &f, 4);
		else memcpy(dst + i * 4, &v, 4);
	}
}

static UINT ColumnStride(UINT r) { return r == 3 ? 16 : r * 4; }

// Write one element of a member from tightly packed source data
static UINT WritePacked(const Member *m, uint8_t *dst, const uint8_t *src, UINT avail)
{
	if (m->type == PT_STRUCT && m->sub) {
		UINT used = 0;
		for (const auto &sm : m->sub->members) {
			UINT n = sm.elements ? sm.elements : 1;
			for (UINT e = 0; e < n && used < avail; e++)
				used += WritePacked(&sm, dst + sm.offset + e * sm.stride, src + used, avail - used);
		}
		return used;
	}
	UINT bytes = std::min(m->packed, avail);
	if (m->matrix) {
		UINT cs = ColumnStride(m->cols);
		for (UINT c = 0; c < m->rows; c++) {
			UINT n = std::min(m->cols * 4, bytes > c * m->cols * 4 ? bytes - c * m->cols * 4 : 0);
			if (n) memcpy(dst + c * cs, src + c * m->cols * 4, n);
		}
	} else if (m->type == PT_BOOL) {
		for (UINT i = 0; i * 4 < bytes; i++) dst[i] = ((const int *)src)[i] != 0;
	} else {
		memcpy(dst, src, bytes);
	}
	return bytes;
}

void WriteParam(const Member *m, uint8_t *data, const void *value, UINT bytes, ParamType srcType, UINT count, bool srcMatrix)
{
	if (!m || !data || !value) return;
	uint8_t *dst = data + m->offset;
	UINT elems = m->elements ? m->elements : 1;
	if (srcMatrix) {
		const float *mf = (const float *)value;
		for (UINT e = 0; e < std::min(count, elems); e++, mf += 16) {
			uint8_t *d = dst + e * m->stride;
			if (!m->matrix) {
				memcpy(d, mf, std::min<UINT>(m->size, 64));
				continue;
			}
			UINT cs = ColumnStride(m->cols);
			for (UINT c = 0; c < m->rows && c < 4; c++)
				for (UINT r = 0; r < m->cols && r < 4; r++)
					memcpy(d + c * cs + r * 4, &mf[c * 4 + r], 4);
		}
		return;
	}
	if (srcType == PT_STRUCT) {
		const uint8_t *s = (const uint8_t *)value;
		UINT used = 0;
		for (UINT e = 0; e < elems && used < bytes; e++)
			used += WritePacked(m, dst + e * m->stride, s + used, bytes - used);
		return;
	}
	if (m->type == PT_STRUCT) {
		WriteParam(m, data, value, bytes, PT_STRUCT, count, false);
		return;
	}
	// scalars and vectors: 'count' components (vectors are given as 4 floats each)
	UINT comps = m->matrix ? m->rows * m->cols : m->cols;
	bool vec4src = bytes == 16 * count && comps != 4 && !m->matrix;
	const uint8_t *s = (const uint8_t *)value;
	for (UINT e = 0; e < elems && e < count; e++) {
		uint8_t *d = dst + e * m->stride;
		UINT n = vec4src ? std::min(comps, 4u) : comps;
		if (m->type == PT_BOOL) {
			for (UINT i = 0; i < n; i++) {
				int v = srcType == PT_FLOAT ? ((const float *)s)[i] != 0.0f : ((const int *)s)[i] != 0;
				d[i] = (uint8_t)v;
			}
		} else if (m->matrix) {
			UINT cs = ColumnStride(m->cols);
			for (UINT c = 0; c < m->rows; c++) WriteScalarArray(d + c * cs, m->type, s + c * m->cols * 4, srcType, m->cols);
		} else {
			WriteScalarArray(d, m->type, s, srcType, n);
		}
		s += vec4src ? 16 : comps * 4;
	}
}

static void ReadParam(const Member *m, const uint8_t *data, void *out, UINT bytes, bool matrix)
{
	const uint8_t *src = data + m->offset;
	if (matrix && m->matrix) {
		float r[16] = {};
		UINT cs = ColumnStride(m->cols);
		for (UINT c = 0; c < m->rows && c < 4; c++)
			for (UINT k = 0; k < m->cols && k < 4; k++)
				memcpy(&r[c * 4 + k], src + c * cs + k * 4, 4);
		memcpy(out, r, std::min<UINT>(bytes, 64));
		return;
	}
	memcpy(out, src, std::min(bytes, m->size));
}

// ---------------------------------------------------------------------------
// Effect

Effect::~Effect()
{
	for (auto &p : params) Rebind(p->texture, (IDirect3DBaseTexture9 *)nullptr);
	if (dev && dev->effect == this) {
		dev->effect = nullptr;
		dev->effectPass = -1;
	}
}

EffectParam *Effect::FindParam(D3DXHANDLE h)
{
	if (!h) return nullptr;
	for (auto &p : params)
		if (p->name.c_str() == h) return p.get();
	for (auto &p : params)
		if (p->name == h) return p.get();
	return nullptr;
}

Technique *Effect::FindTechnique(D3DXHANDLE h)
{
	if (!h) return nullptr;
	for (auto &t : techniques)
		if (t->name.c_str() == h) return t.get();
	for (auto &t : techniques)
		if (t->name == h) return t.get();
	return nullptr;
}

void Effect::ApplyPass(int pass)
{
	if (!current || pass < 0 || pass >= (int)current->passes.size()) return;
	for (const auto &st : current->passes[pass].states)
		dev->rs[st.state] = st.value;
	dev->effect = this;
	dev->effectPass = pass;
}

static bool ParseValue(const std::string &sIn, DWORD &v)
{
	std::string s = sIn;
	std::transform(s.begin(), s.end(), s.begin(), ::tolower);
	static const std::map<std::string, DWORD> names = {
		{"true", 1}, {"false", 0},
		{"zero", D3DBLEND_ZERO}, {"one", D3DBLEND_ONE}, {"srccolor", D3DBLEND_SRCCOLOR}, {"invsrccolor", D3DBLEND_INVSRCCOLOR},
		{"srcalpha", D3DBLEND_SRCALPHA}, {"invsrcalpha", D3DBLEND_INVSRCALPHA}, {"destalpha", D3DBLEND_DESTALPHA},
		{"invdestalpha", D3DBLEND_INVDESTALPHA}, {"destcolor", D3DBLEND_DESTCOLOR}, {"invdestcolor", D3DBLEND_INVDESTCOLOR},
		{"srcalphasat", D3DBLEND_SRCALPHASAT}, {"blendfactor", D3DBLEND_BLENDFACTOR}, {"invblendfactor", D3DBLEND_INVBLENDFACTOR},
		{"add", D3DBLENDOP_ADD}, {"subtract", D3DBLENDOP_SUBTRACT}, {"revsubtract", D3DBLENDOP_REVSUBTRACT},
		{"min", D3DBLENDOP_MIN}, {"max", D3DBLENDOP_MAX},
		{"none", D3DCULL_NONE}, {"cw", D3DCULL_CW}, {"ccw", D3DCULL_CCW},
		{"never", D3DCMP_NEVER}, {"less", D3DCMP_LESS}, {"equal", D3DCMP_EQUAL}, {"lessequal", D3DCMP_LESSEQUAL},
		{"greater", D3DCMP_GREATER}, {"notequal", D3DCMP_NOTEQUAL}, {"greaterequal", D3DCMP_GREATEREQUAL}, {"always", D3DCMP_ALWAYS},
		{"keep", D3DSTENCILOP_KEEP}, {"replace", D3DSTENCILOP_REPLACE}, {"incrsat", D3DSTENCILOP_INCRSAT},
		{"decrsat", D3DSTENCILOP_DECRSAT}, {"invert", D3DSTENCILOP_INVERT}, {"incr", D3DSTENCILOP_INCR}, {"decr", D3DSTENCILOP_DECR},
		{"point", D3DFILL_POINT}, {"wireframe", D3DFILL_WIREFRAME}, {"solid", D3DFILL_SOLID},
		{"red", 1}, {"green", 2}, {"blue", 4}, {"alpha", 8},
	};
	DWORD r = 0;
	bool any = false;
	std::stringstream ss(s);
	std::string tok;
	while (std::getline(ss, tok, '|')) {
		while (!tok.empty() && isspace((unsigned char)tok.front())) tok.erase(0, 1);
		while (!tok.empty() && isspace((unsigned char)tok.back())) tok.pop_back();
		if (tok.empty()) continue;
		auto it = names.find(tok);
		if (it != names.end()) { r |= it->second; any = true; continue; }
		char *end = nullptr;
		if (tok.find('.') != std::string::npos) {
			float f = strtof(tok.c_str(), &end);
			if (end && !*end) { DWORD d; memcpy(&d, &f, 4); r |= d; any = true; continue; }
		}
		unsigned long n = strtoul(tok.c_str(), &end, 0);
		if (end && !*end) { r |= (DWORD)n; any = true; continue; }
		return false;
	}
	v = r;
	return any;
}

static bool ParseState(const std::string &name, D3DRENDERSTATETYPE &st)
{
	static const std::map<std::string, D3DRENDERSTATETYPE> states = {
		{"zenable", D3DRS_ZENABLE}, {"zwriteenable", D3DRS_ZWRITEENABLE}, {"zfunc", D3DRS_ZFUNC},
		{"alphablendenable", D3DRS_ALPHABLENDENABLE}, {"srcblend", D3DRS_SRCBLEND}, {"destblend", D3DRS_DESTBLEND},
		{"blendop", D3DRS_BLENDOP}, {"separatealphablendenable", D3DRS_SEPARATEALPHABLENDENABLE},
		{"srcblendalpha", D3DRS_SRCBLENDALPHA}, {"destblendalpha", D3DRS_DESTBLENDALPHA}, {"blendopalpha", D3DRS_BLENDOPALPHA},
		{"cullmode", D3DRS_CULLMODE}, {"fillmode", D3DRS_FILLMODE},
		{"alphatestenable", D3DRS_ALPHATESTENABLE}, {"alpharef", D3DRS_ALPHAREF}, {"alphafunc", D3DRS_ALPHAFUNC},
		{"stencilenable", D3DRS_STENCILENABLE}, {"stencilfunc", D3DRS_STENCILFUNC}, {"stencilref", D3DRS_STENCILREF},
		{"stencilpass", D3DRS_STENCILPASS}, {"stencilfail", D3DRS_STENCILFAIL}, {"stencilzfail", D3DRS_STENCILZFAIL},
		{"stencilmask", D3DRS_STENCILMASK}, {"stencilwritemask", D3DRS_STENCILWRITEMASK},
		{"twosidedstencilmode", D3DRS_TWOSIDEDSTENCILMODE}, {"ccw_stencilfunc", D3DRS_CCW_STENCILFUNC},
		{"ccw_stencilpass", D3DRS_CCW_STENCILPASS}, {"ccw_stencilfail", D3DRS_CCW_STENCILFAIL}, {"ccw_stencilzfail", D3DRS_CCW_STENCILZFAIL},
		{"colorwriteenable", D3DRS_COLORWRITEENABLE}, {"colorwriteenable1", D3DRS_COLORWRITEENABLE1},
		{"colorwriteenable2", D3DRS_COLORWRITEENABLE2}, {"colorwriteenable3", D3DRS_COLORWRITEENABLE3},
		{"depthbias", D3DRS_DEPTHBIAS}, {"slopescaledepthbias", D3DRS_SLOPESCALEDEPTHBIAS},
		{"scissortestenable", D3DRS_SCISSORTESTENABLE}, {"multisampleantialias", D3DRS_MULTISAMPLEANTIALIAS},
		{"pointspriteenable", D3DRS_POINTSPRITEENABLE}, {"pointscaleenable", D3DRS_POINTSCALEENABLE},
		{"blendfactor", D3DRS_BLENDFACTOR}, {"srgbwriteenable", D3DRS_SRGBWRITEENABLE},
	};
	std::string n = name;
	std::transform(n.begin(), n.end(), n.begin(), ::tolower);
	auto it = states.find(n);
	if (it == states.end()) return false;
	st = it->second;
	return true;
}

static void ParseTechniques(Effect *fx)
{
	std::istringstream in(fx->src.text);
	std::string line;
	Technique *cur = nullptr;
	while (std::getline(in, line)) {
		size_t p = line.find("// @");
		if (p == std::string::npos) continue;
		std::istringstream ls(line.substr(p + 4));
		std::string kind, name;
		ls >> kind >> name;
		if (kind == "technique") {
			fx->techniques.push_back(std::make_unique<Technique>());
			cur = fx->techniques.back().get();
			cur->name = name;
		} else if (kind == "pass" && cur) {
			Pass ps;
			ps.name = name;
			std::string kv;
			while (ls >> kv) {
				size_t eq = kv.find('=');
				if (eq == std::string::npos) continue;
				std::string k = kv.substr(0, eq), v = kv.substr(eq + 1);
				if (k == "vs") ps.vsName = v;
				else if (k == "ps") ps.psName = v;
				else {
					D3DRENDERSTATETYPE st;
					DWORD val;
					if (ParseState(k, st) && ParseValue(v, val)) {
						if (st == D3DRS_ALPHABLENDENABLE || st == D3DRS_ZWRITEENABLE || st == D3DRS_STENCILENABLE) val = val ? 1 : 0;
						ps.states.push_back({st, val});
					} else {
						Log("%s: unknown pass state %s", fx->src.path.c_str(), kv.c_str());
					}
				}
			}
			if (!ps.vsName.empty()) ps.vs = [fx->lib newFunctionWithName:[NSString stringWithUTF8String:ps.vsName.c_str()]];
			if (!ps.psName.empty()) ps.ps = [fx->lib newFunctionWithName:[NSString stringWithUTF8String:ps.psName.c_str()]];
			if (!ps.vs) Log("%s: vertex function %s missing", fx->src.path.c_str(), ps.vsName.c_str());
			cur->passes.push_back(ps);
		}
	}
}

}

using namespace m9;

// ---------------------------------------------------------------------------
// ID3DXBuffer

ID3DXBuffer::ID3DXBuffer(const void *data, DWORD size) : m_data(malloc(size ? size : 1)), m_size(size)
{
	if (data && size) memcpy(m_data, data, size);
}

ID3DXBuffer::~ID3DXBuffer() { free(m_data); }

// ---------------------------------------------------------------------------
// Effects

HRESULT D3DXCreateEffectFromFileA(IDirect3DDevice9 *dev, const char *file, const D3DXMACRO *defines, void *,
	DWORD, ID3DXEffectPool *, ID3DXEffect **effect, ID3DXBuffer **errors)
{
	if (!dev || !file || !effect) return D3DERR_INVALIDCALL;
	Effect *fx = new Effect();
	fx->dev = dev->Impl();
	if (!LoadShaderSource(file, fx->src, defines)) {
		delete fx;
		if (errors) {
			std::string e = std::string("shader source not found: ") + file;
			*errors = new ID3DXBuffer(e.c_str(), (DWORD)e.size() + 1);
		}
		return E_FAIL;
	}
	std::string err;
	fx->lib = CompileLibrary(fx->dev->mtl, fx->src, defines, err);
	if (!fx->lib) {
		if (errors) *errors = new ID3DXBuffer(err.c_str(), (DWORD)err.size() + 1);
		delete fx;
		return E_FAIL;
	}
	auto g = fx->src.structs.find("Globals");
	if (g != fx->src.structs.end()) fx->globals = g->second;
	fx->data.assign(fx->globals.size, 0);
	for (const auto &m : fx->globals.members) {
		auto p = std::make_unique<EffectParam>();
		p->name = m.name;
		p->member = &m;
		fx->params.push_back(std::move(p));
	}
	for (const auto &t : fx->src.textureSlots) {
		auto p = std::make_unique<EffectParam>();
		p->name = t.first;
		p->textureSlot = t.second;
		fx->params.push_back(std::move(p));
	}
	ParseTechniques(fx);
	if (!fx->techniques.empty()) fx->current = fx->techniques[0].get();
	ID3DXEffect *iface = new ID3DXEffect();
	iface->m_effect = fx;
	*effect = iface;
	return S_OK;
}

ID3DXEffect::~ID3DXEffect() { delete m_effect; }

D3DXHANDLE ID3DXEffect::GetParameterByName(D3DXHANDLE parent, const char *name)
{
	EffectParam *p = m_effect->FindParam(name);
	if (!p) Log("%s: no parameter %s", m_effect->src.path.c_str(), name ? name : "(null)");
	return p ? p->name.c_str() : nullptr;
}

D3DXHANDLE ID3DXEffect::GetParameterBySemantic(D3DXHANDLE, const char *) { return nullptr; }

D3DXHANDLE ID3DXEffect::GetParameter(D3DXHANDLE, UINT index)
{
	return index < m_effect->params.size() ? m_effect->params[index]->name.c_str() : nullptr;
}

D3DXHANDLE ID3DXEffect::GetParameterElement(D3DXHANDLE param, UINT index)
{
	EffectParam *p = index == 0 ? m_effect->FindParam(param) : nullptr;
	return p ? p->name.c_str() : nullptr;
}

HRESULT ID3DXEffect::GetParameterDesc(D3DXHANDLE h, D3DXPARAMETER_DESC *desc)
{
	EffectParam *p = m_effect->FindParam(h);
	if (!p || !desc) return D3DERR_INVALIDCALL;
	memset(desc, 0, sizeof(*desc));
	desc->Name = p->name.c_str();
	if (p->member) {
		desc->Rows = p->member->matrix ? p->member->rows : 1;
		desc->Columns = p->member->cols;
		desc->Elements = p->member->elements;
		desc->Bytes = p->member->packed * (p->member->elements ? p->member->elements : 1);
		desc->Class = p->member->matrix ? D3DXPC_MATRIX_ROWS : p->member->cols > 1 ? D3DXPC_VECTOR : D3DXPC_SCALAR;
		desc->Type = p->member->type == PT_FLOAT ? D3DXPT_FLOAT : p->member->type == PT_BOOL ? D3DXPT_BOOL : D3DXPT_INT;
	} else {
		desc->Class = D3DXPC_OBJECT;
		desc->Type = D3DXPT_TEXTURE;
	}
	return S_OK;
}

D3DXHANDLE ID3DXEffect::GetTechniqueByName(const char *name)
{
	Technique *t = m_effect->FindTechnique(name);
	if (!t) Log("%s: no technique %s", m_effect->src.path.c_str(), name ? name : "(null)");
	return t ? t->name.c_str() : nullptr;
}

D3DXHANDLE ID3DXEffect::GetTechnique(UINT index)
{
	return index < m_effect->techniques.size() ? m_effect->techniques[index]->name.c_str() : nullptr;
}

D3DXHANDLE ID3DXEffect::GetCurrentTechnique() { return m_effect->current ? m_effect->current->name.c_str() : nullptr; }

HRESULT ID3DXEffect::SetTechnique(D3DXHANDLE h)
{
	Technique *t = m_effect->FindTechnique(h);
	if (!t) return D3DERR_INVALIDCALL;
	m_effect->current = t;
	return S_OK;
}

HRESULT ID3DXEffect::ValidateTechnique(D3DXHANDLE h) { return m_effect->FindTechnique(h) ? S_OK : E_FAIL; }

HRESULT ID3DXEffect::FindNextValidTechnique(D3DXHANDLE h, D3DXHANDLE *next)
{
	if (!next) return D3DERR_INVALIDCALL;
	auto &v = m_effect->techniques;
	size_t i = 0;
	if (h) {
		Technique *t = m_effect->FindTechnique(h);
		for (; i < v.size() && v[i].get() != t; i++) {}
		i++;
	}
	*next = i < v.size() ? v[i]->name.c_str() : nullptr;
	return S_OK;
}

HRESULT ID3DXEffect::Begin(UINT *passes, DWORD)
{
	if (!m_effect->current) return D3DERR_INVALIDCALL;
	if (passes) *passes = (UINT)m_effect->current->passes.size();
	return S_OK;
}

HRESULT ID3DXEffect::BeginPass(UINT pass)
{
	if (!m_effect->current || pass >= m_effect->current->passes.size()) return D3DERR_INVALIDCALL;
	m_effect->activePass = (int)pass;
	m_effect->ApplyPass((int)pass);
	return S_OK;
}

HRESULT ID3DXEffect::CommitChanges() { return S_OK; }

HRESULT ID3DXEffect::EndPass()
{
	m_effect->activePass = -1;
	return S_OK;
}

HRESULT ID3DXEffect::End() { return S_OK; }

HRESULT ID3DXEffect::GetDevice(IDirect3DDevice9 **dev)
{
	if (!dev) return D3DERR_INVALIDCALL;
	*dev = m_effect->dev->iface;
	(*dev)->AddRef();
	return S_OK;
}

static Member *ParamMember(Effect *fx, D3DXHANDLE h)
{
	EffectParam *p = fx->FindParam(h);
	return p ? const_cast<Member *>(p->member) : nullptr;
}

HRESULT ID3DXEffect::SetValue(D3DXHANDLE h, const void *data, UINT bytes)
{
	EffectParam *p = m_effect->FindParam(h);
	if (!p) return D3DERR_INVALIDCALL;
	if (p->textureSlot >= 0) {
		Rebind(p->texture, data ? *(IDirect3DBaseTexture9 *const *)data : (IDirect3DBaseTexture9 *)nullptr);
		return S_OK;
	}
	const Member *m = p->member;
	if (!m) return D3DERR_INVALIDCALL;
	if (m->type == PT_STRUCT) WriteParam(m, m_effect->data.data(), data, bytes, PT_STRUCT, 1, false);
	else if (m->matrix && bytes >= 64 && bytes % 64 == 0) WriteParam(m, m_effect->data.data(), data, bytes, PT_FLOAT, bytes / 64, true);
	else WriteParam(m, m_effect->data.data(), data, bytes, PT_STRUCT, 1, false);
	return S_OK;
}

HRESULT ID3DXEffect::GetValue(D3DXHANDLE h, void *data, UINT bytes)
{
	const Member *m = ParamMember(m_effect, h);
	if (!m || !data) return D3DERR_INVALIDCALL;
	ReadParam(m, m_effect->data.data(), data, bytes, false);
	return S_OK;
}

HRESULT ID3DXEffect::SetBool(D3DXHANDLE h, WINBOOL b)
{
	int v = b ? 1 : 0;
	return SetIntArray(h, &v, 1);
}

HRESULT ID3DXEffect::GetBool(D3DXHANDLE h, WINBOOL *b)
{
	const Member *m = ParamMember(m_effect, h);
	if (!m || !b) return D3DERR_INVALIDCALL;
	*b = m->type == PT_BOOL ? m_effect->data[m->offset] != 0 : *(const int *)(m_effect->data.data() + m->offset) != 0;
	return S_OK;
}

HRESULT ID3DXEffect::SetBoolArray(D3DXHANDLE h, const WINBOOL *b, UINT count)
{
	std::vector<int> v(count);
	for (UINT i = 0; i < count; i++) v[i] = b[i] ? 1 : 0;
	return SetIntArray(h, v.data(), count);
}

HRESULT ID3DXEffect::SetInt(D3DXHANDLE h, INT n) { return SetIntArray(h, &n, 1); }

HRESULT ID3DXEffect::GetInt(D3DXHANDLE h, INT *n)
{
	const Member *m = ParamMember(m_effect, h);
	if (!m || !n) return D3DERR_INVALIDCALL;
	const uint8_t *d = m_effect->data.data() + m->offset;
	if (m->type == PT_FLOAT) *n = (INT)*(const float *)d;
	else if (m->type == PT_BOOL) *n = d[0];
	else *n = *(const int *)d;
	return S_OK;
}

HRESULT ID3DXEffect::SetIntArray(D3DXHANDLE h, const INT *n, UINT count)
{
	const Member *m = ParamMember(m_effect, h);
	if (!m) return D3DERR_INVALIDCALL;
	UINT comps = m->matrix ? m->rows * m->cols : m->cols;
	UINT elems = (count + comps - 1) / comps;
	WriteParam(m, m_effect->data.data(), n, count * 4, PT_INT, elems ? elems : 1, false);
	return S_OK;
}

HRESULT ID3DXEffect::SetFloat(D3DXHANDLE h, float f) { return SetFloatArray(h, &f, 1); }

HRESULT ID3DXEffect::GetFloat(D3DXHANDLE h, float *f)
{
	const Member *m = ParamMember(m_effect, h);
	if (!m || !f) return D3DERR_INVALIDCALL;
	const uint8_t *d = m_effect->data.data() + m->offset;
	if (m->type == PT_FLOAT) *f = *(const float *)d;
	else *f = (float)*(const int *)d;
	return S_OK;
}

HRESULT ID3DXEffect::SetFloatArray(D3DXHANDLE h, const float *f, UINT count)
{
	const Member *m = ParamMember(m_effect, h);
	if (!m) return D3DERR_INVALIDCALL;
	UINT comps = m->matrix ? m->rows * m->cols : m->cols;
	UINT elems = (count + comps - 1) / comps;
	WriteParam(m, m_effect->data.data(), f, count * 4, PT_FLOAT, elems ? elems : 1, false);
	return S_OK;
}

HRESULT ID3DXEffect::SetVector(D3DXHANDLE h, const D3DXVECTOR4 *v) { return SetVectorArray(h, v, 1); }

HRESULT ID3DXEffect::GetVector(D3DXHANDLE h, D3DXVECTOR4 *v)
{
	const Member *m = ParamMember(m_effect, h);
	if (!m || !v) return D3DERR_INVALIDCALL;
	*v = D3DXVECTOR4(0, 0, 0, 0);
	memcpy(v, m_effect->data.data() + m->offset, std::min<UINT>(16, m->cols * 4));
	return S_OK;
}

HRESULT ID3DXEffect::SetVectorArray(D3DXHANDLE h, const D3DXVECTOR4 *v, UINT count)
{
	const Member *m = ParamMember(m_effect, h);
	if (!m) return D3DERR_INVALIDCALL;
	WriteParam(m, m_effect->data.data(), v, count * 16, PT_FLOAT, count, false);
	return S_OK;
}

HRESULT ID3DXEffect::SetMatrix(D3DXHANDLE h, const D3DXMATRIX *m) { return SetMatrixArray(h, m, 1); }

HRESULT ID3DXEffect::GetMatrix(D3DXHANDLE h, D3DXMATRIX *m)
{
	const Member *mb = ParamMember(m_effect, h);
	if (!mb || !m) return D3DERR_INVALIDCALL;
	D3DXMatrixIdentity(m);
	ReadParam(mb, m_effect->data.data(), m, 64, true);
	return S_OK;
}

HRESULT ID3DXEffect::SetMatrixArray(D3DXHANDLE h, const D3DXMATRIX *m, UINT count)
{
	const Member *mb = ParamMember(m_effect, h);
	if (!mb) return D3DERR_INVALIDCALL;
	WriteParam(mb, m_effect->data.data(), m, count * 64, PT_FLOAT, count, true);
	return S_OK;
}

HRESULT ID3DXEffect::SetMatrixTranspose(D3DXHANDLE h, const D3DXMATRIX *m)
{
	D3DXMATRIX t;
	D3DXMatrixTranspose(&t, m);
	return SetMatrix(h, &t);
}

HRESULT ID3DXEffect::SetTexture(D3DXHANDLE h, IDirect3DBaseTexture9 *tex)
{
	EffectParam *p = m_effect->FindParam(h);
	if (!p || p->textureSlot < 0) return D3DERR_INVALIDCALL;
	Rebind(p->texture, tex);
	return S_OK;
}

HRESULT ID3DXEffect::GetTexture(D3DXHANDLE h, IDirect3DBaseTexture9 **tex)
{
	EffectParam *p = m_effect->FindParam(h);
	if (!p || !tex) return D3DERR_INVALIDCALL;
	*tex = p->texture;
	if (*tex) (*tex)->AddRef();
	return S_OK;
}

HRESULT D3DXDisassembleEffect(ID3DXEffect *, WINBOOL, ID3DXBuffer **out)
{
	if (out) *out = nullptr;
	return E_NOTIMPL;
}

// ---------------------------------------------------------------------------
// Individually compiled shaders and constant tables

HRESULT D3DXCompileShaderFromFileA(const char *file, const D3DXMACRO *defines, void *, const char *function,
	const char *profile, DWORD, ID3DXBuffer **shader, ID3DXBuffer **errors, ID3DXConstantTable **table)
{
	Device *dev = CurrentDevice();
	if (!dev || !file || !function) return D3DERR_INVALIDCALL;
	ShaderSource src;
	if (!LoadShaderSource(file, src, defines)) {
		if (errors) {
			std::string e = std::string("shader source not found: ") + file;
			*errors = new ID3DXBuffer(e.c_str(), (DWORD)e.size() + 1);
		}
		return E_FAIL;
	}
	std::string err;
	id<MTLLibrary> lib = CompileLibrary(dev->mtl, src, defines, err);
	if (!lib) {
		if (errors) *errors = new ID3DXBuffer(err.c_str(), (DWORD)err.size() + 1);
		return E_FAIL;
	}
	auto prog = std::make_shared<Program>();
	prog->fn = [lib newFunctionWithName:[NSString stringWithUTF8String:function]];
	if (!prog->fn) {
		Log("%s: function %s not found", src.path.c_str(), function);
		if (errors) {
			std::string e = std::string("function not found: ") + function;
			*errors = new ID3DXBuffer(e.c_str(), (DWORD)e.size() + 1);
		}
		return E_FAIL;
	}
	prog->vertex = profile && profile[0] == 'v';
	prog->name = function;
	auto c = src.funcConst.find(function);
	if (c != src.funcConst.end() && src.structs.count(c->second)) prog->constants = src.structs[c->second];
	prog->data.assign(prog->constants.size, 0);
	prog->textureSlots = src.textureSlots;
	if (shader) {
		auto *holder = new std::shared_ptr<Program>(prog);
		*shader = new ID3DXBuffer(&holder, sizeof(holder));
	}
	if (table) {
		ID3DXConstantTable *t = new ID3DXConstantTable();
		t->m_table = new ConstantTable();
		t->m_table->prog = prog;
		*table = t;
	}
	return S_OK;
}

static std::shared_ptr<Program> ProgramFromCode(const DWORD *code)
{
	if (!code) return nullptr;
	auto *holder = *(std::shared_ptr<Program> *const *)code;
	return holder ? *holder : nullptr;
}

HRESULT IDirect3DDevice9::CreateVertexShader(const DWORD *function, IDirect3DVertexShader9 **shader)
{
	auto prog = ProgramFromCode(function);
	if (!prog || !shader) return D3DERR_INVALIDCALL;
	IDirect3DVertexShader9 *s = new IDirect3DVertexShader9();
	Shader *impl = new Shader();
	impl->prog = prog;
	s->M9Attach(impl);
	*shader = s;
	return S_OK;
}

HRESULT IDirect3DDevice9::CreatePixelShader(const DWORD *function, IDirect3DPixelShader9 **shader)
{
	auto prog = ProgramFromCode(function);
	if (!prog || !shader) return D3DERR_INVALIDCALL;
	IDirect3DPixelShader9 *s = new IDirect3DPixelShader9();
	Shader *impl = new Shader();
	impl->prog = prog;
	s->M9Attach(impl);
	*shader = s;
	return S_OK;
}

HRESULT D3DXGetShaderConstantTable(const DWORD *function, ID3DXConstantTable **table)
{
	auto prog = ProgramFromCode(function);
	if (!prog || !table) return D3DERR_INVALIDCALL;
	ID3DXConstantTable *t = new ID3DXConstantTable();
	t->m_table = new ConstantTable();
	t->m_table->prog = prog;
	*table = t;
	return S_OK;
}

HRESULT D3DXDisassembleShader(const DWORD *, WINBOOL, const char *, ID3DXBuffer **out)
{
	if (out) *out = nullptr;
	return E_NOTIMPL;
}

ID3DXConstantTable::~ID3DXConstantTable() { delete m_table; }

D3DXHANDLE ID3DXConstantTable::GetConstantByName(D3DXHANDLE, const char *name)
{
	if (!m_table || !name) return nullptr;
	Program *p = m_table->prog.get();
	if (const Member *m = p->constants.Find(name)) return m->name.c_str();
	auto it = p->textureSlots.find(name);
	if (it != p->textureSlots.end()) return it->first.c_str();
	return nullptr;
}

D3DXHANDLE ID3DXConstantTable::GetConstant(D3DXHANDLE, UINT index)
{
	Program *p = m_table ? m_table->prog.get() : nullptr;
	if (!p || index >= p->constants.members.size()) return nullptr;
	return p->constants.members[index].name.c_str();
}

static const Member *TableMember(ConstantTable *t, D3DXHANDLE h)
{
	if (!t || !h) return nullptr;
	return t->prog->constants.Find(h);
}

HRESULT ID3DXConstantTable::GetConstantDesc(D3DXHANDLE h, D3DXCONSTANT_DESC *desc, UINT *count)
{
	if (!desc) return D3DERR_INVALIDCALL;
	memset(desc, 0, sizeof(*desc));
	if (const Member *m = TableMember(m_table, h)) {
		desc->Name = m->name.c_str();
		desc->RegisterSet = D3DXRS_FLOAT4;
		desc->RegisterIndex = m->offset / 16;
		desc->RegisterCount = (m->size + 15) / 16;
		desc->Rows = m->matrix ? m->rows : 1;
		desc->Columns = m->cols;
		desc->Elements = m->elements ? m->elements : 1;
		desc->Bytes = m->packed * desc->Elements;
		desc->Class = m->matrix ? D3DXPC_MATRIX_ROWS : m->cols > 1 ? D3DXPC_VECTOR : D3DXPC_SCALAR;
		desc->Type = m->type == PT_FLOAT ? D3DXPT_FLOAT : m->type == PT_BOOL ? D3DXPT_BOOL : D3DXPT_INT;
	} else {
		for (const auto &t : m_table->prog->textureSlots) {
			if (t.first == h) {
				desc->Name = t.first.c_str();
				desc->RegisterSet = D3DXRS_SAMPLER;
				desc->RegisterIndex = (UINT)t.second;
				desc->RegisterCount = 1;
				desc->Class = D3DXPC_OBJECT;
				desc->Type = D3DXPT_SAMPLER2D;
				break;
			}
		}
		if (!desc->Name) return D3DERR_INVALIDCALL;
	}
	if (count) *count = 1;
	return S_OK;
}

UINT ID3DXConstantTable::GetSamplerIndex(D3DXHANDLE h)
{
	if (!m_table || !h) return (UINT)-1;
	for (const auto &t : m_table->prog->textureSlots)
		if (t.first == h) return (UINT)t.second;
	return (UINT)-1;
}

HRESULT ID3DXConstantTable::SetValue(IDirect3DDevice9 *, D3DXHANDLE h, const void *data, UINT bytes)
{
	const Member *m = TableMember(m_table, h);
	if (!m) return D3DERR_INVALIDCALL;
	Program *p = m_table->prog.get();
	if (m->matrix && bytes % 64 == 0 && bytes) WriteParam(m, p->data.data(), data, bytes, PT_FLOAT, bytes / 64, true);
	else WriteParam(m, p->data.data(), data, bytes, PT_STRUCT, 1, false);
	return S_OK;
}

HRESULT ID3DXConstantTable::SetBool(IDirect3DDevice9 *d, D3DXHANDLE h, WINBOOL b)
{
	int v = b ? 1 : 0;
	const Member *m = TableMember(m_table, h);
	if (!m) return D3DERR_INVALIDCALL;
	WriteParam(m, m_table->prog->data.data(), &v, 4, PT_INT, 1, false);
	return S_OK;
}

HRESULT ID3DXConstantTable::SetInt(IDirect3DDevice9 *, D3DXHANDLE h, INT n)
{
	const Member *m = TableMember(m_table, h);
	if (!m) return D3DERR_INVALIDCALL;
	WriteParam(m, m_table->prog->data.data(), &n, 4, PT_INT, 1, false);
	return S_OK;
}

HRESULT ID3DXConstantTable::SetFloat(IDirect3DDevice9 *, D3DXHANDLE h, float f)
{
	const Member *m = TableMember(m_table, h);
	if (!m) return D3DERR_INVALIDCALL;
	WriteParam(m, m_table->prog->data.data(), &f, 4, PT_FLOAT, 1, false);
	return S_OK;
}

HRESULT ID3DXConstantTable::SetFloatArray(IDirect3DDevice9 *, D3DXHANDLE h, const float *f, UINT count)
{
	const Member *m = TableMember(m_table, h);
	if (!m) return D3DERR_INVALIDCALL;
	UINT comps = m->matrix ? m->rows * m->cols : m->cols;
	WriteParam(m, m_table->prog->data.data(), f, count * 4, PT_FLOAT, (count + comps - 1) / comps, false);
	return S_OK;
}

HRESULT ID3DXConstantTable::SetVector(IDirect3DDevice9 *d, D3DXHANDLE h, const D3DXVECTOR4 *v) { return SetVectorArray(d, h, v, 1); }

HRESULT ID3DXConstantTable::SetVectorArray(IDirect3DDevice9 *, D3DXHANDLE h, const D3DXVECTOR4 *v, UINT count)
{
	const Member *m = TableMember(m_table, h);
	if (!m) return D3DERR_INVALIDCALL;
	WriteParam(m, m_table->prog->data.data(), v, count * 16, PT_FLOAT, count, false);
	return S_OK;
}

HRESULT ID3DXConstantTable::SetMatrix(IDirect3DDevice9 *d, D3DXHANDLE h, const D3DXMATRIX *mx) { return SetMatrixArray(d, h, mx, 1); }

HRESULT ID3DXConstantTable::SetMatrixArray(IDirect3DDevice9 *, D3DXHANDLE h, const D3DXMATRIX *mx, UINT count)
{
	const Member *m = TableMember(m_table, h);
	if (!m) return D3DERR_INVALIDCALL;
	WriteParam(m, m_table->prog->data.data(), mx, count * 64, PT_FLOAT, count, true);
	return S_OK;
}

HRESULT ID3DXConstantTable::SetIntArray(IDirect3DDevice9 *, D3DXHANDLE h, const INT *n, UINT count)
{
	const Member *m = TableMember(m_table, h);
	if (!m) return D3DERR_INVALIDCALL;
	UINT comps = m->matrix ? m->rows * m->cols : m->cols;
	WriteParam(m, m_table->prog->data.data(), n, count * 4, PT_INT, (count + comps - 1) / comps, false);
	return S_OK;
}

HRESULT ID3DXConstantTable::SetBoolArray(IDirect3DDevice9 *d, D3DXHANDLE h, const WINBOOL *b, UINT count)
{
	std::vector<INT> v(count);
	for (UINT i = 0; i < count; i++) v[i] = b[i] ? 1 : 0;
	return SetIntArray(d, h, v.data(), count);
}

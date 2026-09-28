// not upstream: see VkShader.h

#include "VkShader.h"
#include "Log.h"
#include <glslang/Public/ShaderLang.h>
#include <glslang/Public/ResourceLimits.h>
#include <glslang/SPIRV/GlslangToSpv.h>
#include <algorithm>
#include <cstring>
#include <cstdlib>
#include <cctype>
#include <mutex>

static std::mutex glslLock; // glslang's process state is shared by all compiles
static bool glslInit = false;

// source files

static bool ReadText (const std::string &path, std::string &out)
{
	FILE *f = fopen (path.c_str(), "rb");
	if (!f) return false;
	fseek (f, 0, SEEK_END);
	long n = ftell (f);
	fseek (f, 0, SEEK_SET);
	out.resize (n > 0 ? n : 0);
	size_t r = n > 0 ? fread (&out[0], 1, n, f) : 0;
	fclose (f);
	out.resize (r);
	return true;
}

static bool ExpandIncludes (const std::string &path, std::string &out, int depth)
{
	std::string text;
	if (depth > 16) { LogErr("Shader includes nested too deep: %s", path.c_str()); return false; }
	if (!ReadText (path, text)) { LogErr("Shader file not found: %s", path.c_str()); return false; }
	std::string dir = path.substr (0, path.find_last_of ('/') + 1);
	out += "#line 1 \"" + path + "\"\n";
	size_t pos = 0;
	int line = 1;
	while (pos < text.size()) {
		size_t e = text.find ('\n', pos);
		if (e == std::string::npos) e = text.size();
		std::string l = text.substr (pos, e - pos);
		if (!l.empty() && l.back() == '\r') l.pop_back();
		size_t k = l.find_first_not_of (" \t");
		if (k != std::string::npos && l.compare (k, 8, "#include") == 0) {
			size_t a = l.find ('"', k);
			size_t b = (a == std::string::npos) ? a : l.find ('"', a + 1);
			if (b != std::string::npos) {
				if (!ExpandIncludes (dir + l.substr (a + 1, b - a - 1), out, depth + 1)) return false;
				out += "#line " + std::to_string (line + 1) + " \"" + path + "\"\n";
				pos = e + 1;
				line++;
				continue;
			}
		}
		out += l;
		out += '\n';
		pos = e + 1;
		line++;
	}
	return true;
}

bool VkLoadShaderSource (const char *file, std::string &src)
{
	src.clear ();
	return ExpandIncludes (file, src, 0);
}

// constant tables

VkConstHandle VkConstTable::GetConstantByName (const char *name) const
{
	for (auto &e : entries) if (e.name == name) return &e;
	return NULL;
}

void VkConstTable::Merge (const VkConstTable &t)
{
	for (auto &e : t.entries) if (!GetConstantByName (e.name.c_str())) entries.push_back (e);
	for (auto &b : t.blocks) if (blocks[b.first] < b.second) blocks[b.first] = b.second;
}

// bytes of a block member in the scalar layout, by its GL type
static UINT GLTypeBytes (int t)
{
	switch (t) {
	case 0x1406: case 0x1404: case 0x1405: case 0x8B56: return 4;  // float, int, uint, bool
	case 0x8B50: case 0x8B53: case 0x8DC6: case 0x8B57: return 8;  // vec2
	case 0x8B51: case 0x8B54: case 0x8DC7: case 0x8B58: return 12; // vec3
	case 0x8B52: case 0x8B55: case 0x8DC8: case 0x8B59: return 16; // vec4
	case 0x8B5A: return 16; // mat2
	case 0x8B5B: return 36; // mat3
	case 0x8B5C: return 64; // mat4
	case 0x8B65: case 0x8B67: return 24; // mat2x3, mat3x2
	case 0x8B66: case 0x8B69: return 32; // mat2x4, mat4x2
	case 0x8B68: case 0x8B6A: return 48; // mat3x4, mat4x3
	}
	return 0;
}

static bool GLTypeSampler (int t, VkImageViewType *vt)
{
	switch (t) {
	case 0x8B5E: case 0x8B62: *vt = VK_IMAGE_VIEW_TYPE_2D; return true; // sampler2D, sampler2DShadow
	case 0x8B60: *vt = VK_IMAGE_VIEW_TYPE_CUBE; return true;           // samplerCube
	case 0x8B5F: *vt = VK_IMAGE_VIEW_TYPE_3D; return true;             // sampler3D
	case 0x8DC1: *vt = VK_IMAGE_VIEW_TYPE_2D_ARRAY; return true;       // sampler2DArray
	}
	return false;
}

// top level constants (struct and array members folded into their variable) and samplers
static void BuildTable (glslang::TProgram &prog, VkConstTable *t)
{
	int nb = prog.getNumUniformBlocks ();
	for (int i = 0; i < nb; i++) {
		const glslang::TObjectReflection &b = prog.getUniformBlock (i);
		int bind = b.getBinding ();
		if (t->blocks[bind] < (UINT)b.size) t->blocks[bind] = b.size;
	}
	int n = prog.getNumUniformVariables ();
	for (int i = 0; i < n; i++) {
		const glslang::TObjectReflection &u = prog.getUniform (i);
		VkImageViewType vt;
		if (GLTypeSampler (u.glDefineType, &vt)) {
			if (!t->GetConstantByName (u.name.c_str())) t->entries.push_back ({ u.name, u.getBinding(), 0, 0, true, vt });
			continue;
		}
		if (u.index < 0 || u.offset < 0) continue;
		std::string top = u.name.substr (0, u.name.find_first_of (".["));
		UINT cnt = u.size > 1 ? u.size : 1;
		UINT tcnt = (u.name.find ('.') != std::string::npos && u.topLevelArraySize > 1) ? u.topLevelArraySize : 1; // arrays of structs reflect element 0's members
		UINT end = u.offset + (cnt - 1) * u.arrayStride + (tcnt - 1) * u.topLevelArrayStride + GLTypeBytes (u.glDefineType);
		int bind = prog.getUniformBlock (u.index).getBinding ();
		VkConstEntry *e = NULL;
		for (auto &x : t->entries) if (!x.sampler && x.name == top) { e = &x; break; }
		if (!e) {
			t->entries.push_back ({ top, bind, (UINT)u.offset, end - u.offset, false, VK_IMAGE_VIEW_TYPE_2D });
			continue;
		}
		UINT s = std::min (e->offset, (UINT)u.offset);
		UINT eEnd = std::max (e->offset + e->size, end);
		e->offset = s;
		e->size = eEnd - s;
	}
}

// compiling

bool VkCompileGLSL (const char *file, const std::string &src, const char *entry, VkShaderStageFlagBits stage,
	const VkMacros &macros, std::vector<uint32_t> &spirv, VkConstTable *table)
{
	std::lock_guard<std::mutex> lock (glslLock);
	if (!glslInit) { glslang::InitializeProcess (); glslInit = true; }

	EShLanguage lang = (stage == VK_SHADER_STAGE_VERTEX_BIT) ? EShLangVertex : EShLangFragment;
	std::string pre = "#version 460\n"
		"#extension GL_GOOGLE_cpp_style_line_directive : require\n"
		"#extension GL_EXT_scalar_block_layout : require\n";
	pre += (lang == EShLangVertex) ? "#define STAGE_VS 1\n" : "#define STAGE_PS 1\n"; // guards fragment-only code in shared functions
	if (entry && entry[0]) pre += std::string ("#define ") + (lang == EShLangVertex ? "VS_" : "PS_") + entry + " 1\n";
	for (auto &m : macros) pre += "#define " + m.first + " " + m.second + "\n";
	const char *strs[2] = { pre.c_str(), src.c_str() };
	const int lens[2] = { (int)pre.size(), (int)src.size() };
	const char *names[2] = { "defines", file };

	glslang::TShader sh (lang);
	sh.setStringsWithLengthsAndNames (strs, lens, names, 2);
	sh.setEnvInput (glslang::EShSourceGlsl, lang, glslang::EShClientVulkan, 100);
	sh.setEnvClient (glslang::EShClientVulkan, glslang::EShTargetVulkan_1_3);
	sh.setEnvTarget (glslang::EShTargetSpv, glslang::EShTargetSpv_1_6);
	EShMessages msg = (EShMessages)(EShMsgSpvRules | EShMsgVulkanRules);
	if (!sh.parse (GetDefaultResources (), 460, false, msg)) {
		LogErr("Shader %s (%s): %s", file, entry ? entry : "", sh.getInfoLog ());
		return false;
	}
	glslang::TProgram prog;
	prog.addShader (&sh);
	if (!prog.link (msg)) {
		LogErr("Shader %s (%s): %s", file, entry ? entry : "", prog.getInfoLog ());
		return false;
	}
	if (table) {
		prog.buildReflection (EShReflectionAllBlockVariables);
		BuildTable (prog, table);
	}
	spirv.clear ();
	glslang::GlslangToSpv (*prog.getIntermediate (lang), spirv);
	return !spirv.empty ();
}

VkShaderEXT VkCreateShaderObject (VkDev *dev, const std::vector<uint32_t> &spirv, VkShaderStageFlagBits stage)
{
	VkDescriptorSetLayout sl = dev->SetLayout ();
	VkPushConstantRange pr = { VK_SHADER_STAGE_VERTEX_BIT | VK_SHADER_STAGE_FRAGMENT_BIT, 0, VkDev::PUSHCONST };
	VkShaderCreateInfoEXT ci = { VK_STRUCTURE_TYPE_SHADER_CREATE_INFO_EXT };
	ci.stage = stage;
	ci.nextStage = (stage == VK_SHADER_STAGE_VERTEX_BIT) ? VK_SHADER_STAGE_FRAGMENT_BIT : 0;
	ci.codeType = VK_SHADER_CODE_TYPE_SPIRV_EXT;
	ci.codeSize = spirv.size() * sizeof(uint32_t);
	ci.pCode = spirv.data();
	ci.pName = "main";
	ci.setLayoutCount = 1;
	ci.pSetLayouts = &sl;
	ci.pushConstantRangeCount = 1;
	ci.pPushConstantRanges = &pr;
	VkShaderEXT sh = VK_NULL_HANDLE;
	VKCHECK(vkx.CreateShadersEXT (dev->dev, 1, &ci, NULL, &sh));
	return sh;
}

// constant buffer

static const VkSamplerDesc defSampler = { VK_FILTER_LINEAR, VK_FILTER_LINEAR, VK_SAMPLER_MIPMAP_MODE_LINEAR,
	VK_SAMPLER_ADDRESS_MODE_REPEAT, VK_SAMPLER_ADDRESS_MODE_REPEAT, VK_SAMPLER_ADDRESS_MODE_REPEAT, 0.0f, 0.0f, false };

VkConstBuffer::VkConstBuffer (VkDev *_dev) : dev(_dev), dirty(true)
{
	dev->RegisterConstants (this, true);
}

VkConstBuffer::~VkConstBuffer ()
{
	dev->RegisterConstants (this, false);
}

void VkConstBuffer::SetTable (const VkConstTable *t)
{
	for (auto &b : t->blocks) {
		auto &d = data[b.first];
		if (d.size() < b.second) d.resize (b.second, 0);
	}
}

void VkConstBuffer::SetValue (VkConstHandle h, const void *p, UINT bytes)
{
	if (!h || h->sampler || !p) return;
	auto it = data.find (h->binding);
	if (it == data.end()) return;
	UINT n = std::min (bytes, h->size);
	if (h->offset + n > it->second.size()) return;
	memcpy (it->second.data() + h->offset, p, n);
	dirty = true;
}

void VkConstBuffer::GetValue (VkConstHandle h, void *p, UINT bytes) const
{
	if (!h || h->sampler || !p) return;
	auto it = data.find (h->binding);
	if (it == data.end()) return;
	UINT n = std::min (bytes, h->size);
	if (h->offset + n > it->second.size()) return;
	memcpy (p, it->second.data() + h->offset, n);
}

void VkConstBuffer::SetTexture (int binding, VkTex *t, const VkSamplerDesc &s)
{
	std::lock_guard<std::mutex> lock (dev->ConstLock ());
	tex[binding] = { t, s };
	dirty = true;
}

VkTex *VkConstBuffer::GetTexture (int binding) const
{
	std::lock_guard<std::mutex> lock (dev->ConstLock ());
	auto it = tex.find (binding);
	return it == tex.end() ? NULL : it->second.tex;
}

void VkConstBuffer::DropTexture (const VkTex *t)
{
	for (auto it = tex.begin(); it != tex.end(); ) {
		if (it->second.tex == t) it = tex.erase (it), dirty = true;
		else ++it;
	}
}

void VkConstBuffer::ClearTextures ()
{
	std::lock_guard<std::mutex> lock (dev->ConstLock ());
	tex.clear ();
	dirty = true;
}

void VkConstBuffer::Push (const std::vector<VkSamplerSlot> &samplers)
{
	if (!dev->IsRecording ()) return;
	dirty = false;
	VkWriteDescriptorSet w[VkDev::MAXBINDINGS];
	VkDescriptorBufferInfo bi[VkDev::NUBOS];
	VkDescriptorImageInfo ii[VkDev::MAXBINDINGS];
	UINT n = 0, nb = 0, ni = 0;
	for (auto &d : data) {
		if (d.first < 0 || d.first >= VkDev::NUBOS || d.second.empty()) continue;
		void *p;
		VkDeviceSize ofs = dev->AllocTransient (d.second.size(), 16, &p);
		memcpy (p, d.second.data(), d.second.size());
		bi[nb] = { dev->TransientBuffer (), ofs, d.second.size() };
		w[n] = { VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET };
		w[n].dstBinding = d.first;
		w[n].descriptorCount = 1;
		w[n].descriptorType = VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER;
		w[n].pBufferInfo = &bi[nb];
		n++, nb++;
	}
	std::lock_guard<std::mutex> lock (dev->ConstLock ());
	for (auto &s : samplers) {
		if (s.binding < VkDev::NUBOS || s.binding >= VkDev::MAXBINDINGS) continue;
		auto it = tex.find (s.binding);
		bool set = (it != tex.end() && it->second.tex && it->second.tex->img);
		VkTex *t = set ? it->second.tex : dev->DefaultTexture (s.view);
		dev->PrepareSample (t);
		ii[ni] = { dev->Sampler (set ? it->second.s : defSampler), t->view, VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL };
		w[n] = { VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET };
		w[n].dstBinding = s.binding;
		w[n].descriptorCount = 1;
		w[n].descriptorType = VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER;
		w[n].pImageInfo = &ii[ni];
		n++, ni++;
	}
	if (n) vkCmdPushDescriptorSet (dev->Cmd (), VK_PIPELINE_BIND_POINT_GRAPHICS, dev->PipelineLayout (), 0, n, w);
}

// effect declarations

static bool IsId (char c) { return isalnum ((unsigned char)c) || c == '_'; }

static std::string Lower (std::string s)
{
	for (auto &c : s) c = (char)tolower ((unsigned char)c);
	return s;
}

static std::string Trim (const std::string &s)
{
	size_t a = s.find_first_not_of (" \t\r\n");
	if (a == std::string::npos) return "";
	size_t b = s.find_last_not_of (" \t\r\n");
	return s.substr (a, b - a + 1);
}

// copy of the source with comments turned into spaces, so offsets match
static std::string StripComments (const std::string &s)
{
	std::string r = s;
	size_t i = 0;
	while (i < r.size()) {
		if (r[i] == '/' && i + 1 < r.size() && r[i+1] == '/') {
			while (i < r.size() && r[i] != '\n') r[i++] = ' ';
		}
		else if (r[i] == '/' && i + 1 < r.size() && r[i+1] == '*') {
			while (i < r.size() && !(r[i] == '*' && i + 1 < r.size() && r[i+1] == '/')) { if (r[i] != '\n') r[i] = ' '; i++; }
			if (i < r.size()) r[i++] = ' ';
			if (i < r.size()) r[i++] = ' ';
		}
		else if (r[i] == '#') { // preprocessor lines (#line "file") are left as they are
			while (i < r.size() && r[i] != '\n') i++;
		}
		else i++;
	}
	return r;
}

static void SkipSpace (const std::string &s, size_t &i)
{
	while (i < s.size() && isspace ((unsigned char)s[i])) i++;
}

static std::string ReadId (const std::string &s, size_t &i)
{
	SkipSpace (s, i);
	size_t a = i;
	while (i < s.size() && IsId (s[i])) i++;
	return s.substr (a, i - a);
}

static size_t MatchBrace (const std::string &s, size_t open)
{
	int d = 0;
	for (size_t i = open; i < s.size(); i++) {
		if (s[i] == '{') d++;
		else if (s[i] == '}' && --d == 0) return i;
	}
	return std::string::npos;
}

static std::string Newlines (const std::string &s, size_t a, size_t b)
{
	return std::string (std::count (s.begin() + a, s.begin() + b, '\n'), '\n');
}

// "key = value;" statements of a block
static std::vector<std::pair<std::string, std::string>> Statements (const std::string &body)
{
	std::vector<std::pair<std::string, std::string>> r;
	size_t a = 0;
	while (a < body.size()) {
		size_t e = body.find (';', a);
		if (e == std::string::npos) e = body.size();
		std::string st = body.substr (a, e - a);
		size_t q = st.find ('=');
		if (q != std::string::npos) r.push_back ({ Trim (st.substr (0, q)), Trim (st.substr (q + 1)) });
		a = e + 1;
	}
	return r;
}

static VkFilter Filter (const std::string &v, bool *aniso)
{
	std::string l = Lower (v);
	if (l == "anisotropic") { if (aniso) *aniso = true; return VK_FILTER_LINEAR; }
	return (l == "point" || l == "none") ? VK_FILTER_NEAREST : VK_FILTER_LINEAR;
}

static VkSamplerAddressMode Address (const std::string &v)
{
	std::string l = Lower (v);
	if (l == "clamp") return VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE;
	if (l == "mirror") return VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT;
	if (l == "border") return VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER;
	if (l == "mirroronce") return VK_SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE;
	return VK_SAMPLER_ADDRESS_MODE_REPEAT;
}

// "compile vs_3_0 Name(args)"
static void ParseCompile (const std::string &v, std::string &name, std::string &args)
{
	size_t i = 0;
	std::string w = ReadId (v, i);
	if (w == "compile") { ReadId (v, i); name = ReadId (v, i); }
	else name = w;
	size_t a = v.find ('(', i), b = v.rfind (')');
	args = (a != std::string::npos && b != std::string::npos && b > a) ? Trim (v.substr (a + 1, b - a - 1)) : "";
}

VkEffect::VkEffect (VkDev *_dev) : dev(_dev), cb(_dev)
{
	cur = NULL;
	curPass = -1;
	saved = false;
	savedState = dev->GetState ();
}

VkEffect::~VkEffect ()
{
	VkDevice d = dev->dev;
	for (auto &t : tech) for (auto &p : t.pass) {
		VkShaderEXT vs = p.vsObj, ps = p.psObj;
		dev->Defer ([d, vs, ps]() {
			if (vs) vkx.DestroyShaderEXT (d, vs, NULL);
			if (ps) vkx.DestroyShaderEXT (d, ps, NULL);
		});
	}
}

VkEffect *VkEffect::Create (VkDev *dev, const char *file, const VkMacros &macros)
{
	VkEffect *fx = new VkEffect (dev);
	if (!fx->Parse (file, macros)) {
		delete fx;
		return NULL;
	}
	return fx;
}

void VkEffect::ParseTechnique (const std::string &code, size_t &i)
{
	Technique t;
	t.name = ReadId (code, i);
	size_t open = code.find ('{', i);
	size_t close = (open == std::string::npos) ? open : MatchBrace (code, open);
	if (close == std::string::npos) { i = code.size(); return; }
	size_t k = open + 1;
	while (k < close) {
		if (!IsId (code[k]) || IsId (code[k-1])) { k++; continue; }
		std::string w = ReadId (code, k);
		if (w != "pass") continue;
		ReadId (code, k);
		size_t po = code.find ('{', k);
		size_t pc = (po == std::string::npos || po > close) ? std::string::npos : MatchBrace (code, po);
		if (pc == std::string::npos) break;
		Pass p;
		p.vsObj = p.psObj = VK_NULL_HANDLE;
		p.failed = false;
		for (auto &s : Statements (code.substr (po + 1, pc - po - 1))) {
			std::string key = Lower (s.first);
			if (key == "vertexshader") ParseCompile (s.second, p.vs, p.vsArgs);
			else if (key == "pixelshader") ParseCompile (s.second, p.ps, p.psArgs);
			else p.states.push_back (s);
		}
		t.pass.push_back (p);
		k = pc + 1;
	}
	tech.push_back (t);
	i = close + 1;
}

bool VkEffect::Parse (const char *_file, const VkMacros &_macros)
{
	file = _file;
	macros = _macros;
	if (!VkLoadShaderSource (_file, src)) return false;
	std::string code = StripComments (src);
	struct Edit { size_t a, b; std::string text; };
	std::vector<Edit> edits;
	std::string reflect;
	const size_t npos = std::string::npos;
	size_t i = 0;
	while (i < code.size()) {
		if (code[i] == '#') { while (i < code.size() && code[i] != '\n') i++; continue; }
		if (!IsId (code[i]) || (i > 0 && IsId (code[i-1]))) { i++; continue; }
		size_t start = i;
		std::string w = ReadId (code, i);
		if (w == "technique") {
			ParseTechnique (code, i);
			edits.push_back ({ start, i, Newlines (code, start, i) });
		}
		else if (w == "texture") { // "uniform extern texture name;" (texture() lookups don't match)
			size_t k = i;
			std::string name = ReadId (code, k);
			SkipSpace (code, k);
			if (name.empty() || k >= code.size() || code[k] != ';') continue;
			size_t ls = code.rfind ('\n', start);
			ls = (ls == npos) ? 0 : ls + 1;
			textures.push_back (name);
			edits.push_back ({ ls, k + 1, "" });
			i = k + 1;
		}
		else if (w == "sampler_state") { // "<type> <name> = sampler_state { ... };"
			size_t k = start;
			while (k > 0 && isspace ((unsigned char)code[k-1])) k--;
			if (k == 0 || code[k-1] != '=') continue;
			k--;
			while (k > 0 && isspace ((unsigned char)code[k-1])) k--;
			size_t ne = k;
			while (k > 0 && IsId (code[k-1])) k--;
			std::string name = code.substr (k, ne - k);
			while (k > 0 && isspace ((unsigned char)code[k-1])) k--;
			size_t te = k;
			while (k > 0 && IsId (code[k-1])) k--;
			std::string type = code.substr (k, te - k);
			size_t open = code.find ('{', i);
			size_t close = (open == npos) ? npos : MatchBrace (code, open);
			size_t semi = (close == npos) ? npos : code.find (';', close);
			if (semi == npos) { LogErr("VkEffect %s: sampler_state of %s not closed", _file, name.c_str()); return false; }
			Sampler s;
			s.name = name;
			s.binding = VkDev::NUBOS + (int)samp.size();
			s.view = (type == "samplerCube") ? VK_IMAGE_VIEW_TYPE_CUBE : (type == "sampler3D" ? VK_IMAGE_VIEW_TYPE_3D : VK_IMAGE_VIEW_TYPE_2D);
			s.desc = defSampler;
			bool aniso = false;
			float maxAniso = 1.0f;
			for (auto &st : Statements (code.substr (open + 1, close - open - 1))) {
				std::string key = Lower (st.first), v = st.second;
				for (auto &m : macros) if (m.first == v) v = m.second; // ANISOTROPY_MACRO
				if (key == "texture") {
					size_t a = v.find ('<'), b = v.find ('>');
					s.texture = Trim ((a != npos && b != npos) ? v.substr (a + 1, b - a - 1) : v);
				}
				else if (key == "minfilter") s.desc.min = Filter (v, &aniso);
				else if (key == "magfilter") s.desc.mag = Filter (v, NULL);
				else if (key == "mipfilter") {
					s.desc.mip = (Lower (v) == "point") ? VK_SAMPLER_MIPMAP_MODE_NEAREST : VK_SAMPLER_MIPMAP_MODE_LINEAR;
					s.desc.noMip = (Lower (v) == "none");
				}
				else if (key == "addressu") s.desc.u = Address (v);
				else if (key == "addressv") s.desc.v = Address (v);
				else if (key == "addressw") s.desc.w = Address (v);
				else if (key == "maxanisotropy") maxAniso = (float)atof (v.c_str());
				else if (key == "mipmaplodbias") s.desc.mipBias = (float)atof (v.c_str());
				else LogWrn("VkEffect %s: sampler state %s not handled", _file, st.first.c_str());
			}
			s.desc.aniso = aniso ? std::max (1.0f, maxAniso) : 0.0f;
			if (s.binding >= VkDev::MAXBINDINGS) { LogErr("VkEffect %s: more than %d samplers", _file, VkDev::MAXBINDINGS - VkDev::NUBOS); return false; }
			s.state = s.desc;
			samp.push_back (s);
			edits.push_back ({ k, semi + 1, "layout(binding = " + std::to_string (s.binding) + ") uniform " + type + " " + name + ";" + Newlines (code, k, semi + 1) });
			i = semi + 1;
		}
		else if (w == "uniform") { // uniform blocks: the reflection pass below references a member of each
			size_t k = i;
			std::string bn = ReadId (code, k);
			SkipSpace (code, k);
			if (bn.empty() || k >= code.size() || code[k] != '{') continue;
			size_t close = MatchBrace (code, k);
			size_t semi = code.find (';', k);
			if (close == npos || semi == npos || semi > close) continue;
			std::string decl = code.substr (k + 1, semi - k - 1);
			size_t b = decl.find ('[');
			if (b != npos) decl = decl.substr (0, b);
			decl = Trim (decl);
			size_t sp = decl.find_last_of (" \t\n");
			std::string member = (sp == npos) ? decl : decl.substr (sp + 1);
			size_t q = close + 1;
			std::string inst = ReadId (code, q);
			if (!member.empty()) reflect += " " + (inst.empty() ? "" : inst + ".") + member + ";";
			i = close + 1;
		}
	}
	for (auto e = edits.rbegin(); e != edits.rend(); ++e) src.replace (e->a, e->b - e->a, e->text);

	// constant table: a fragment stage that references every uniform block
	std::string rsrc = src + "\n#ifdef VKFX_REFLECT\nlayout(location = 0) out vec4 vkfxOut;\nvoid main()\n{\n\tvkfxOut = vec4(0.0);" + reflect + "\n}\n#endif\n";
	VkMacros m = macros;
	m.push_back ({ "VKFX_REFLECT", "1" });
	std::vector<uint32_t> spv;
	if (!VkCompileGLSL (_file, rsrc, "", VK_SHADER_STAGE_FRAGMENT_BIT, m, spv, &table)) return false;
	for (auto &t : textures) table.entries.push_back ({ t, -1, 0, 0, true, VK_IMAGE_VIEW_TYPE_2D });
	for (auto &s : samp) table.entries.push_back ({ s.name, s.binding, 0, 0, true, s.view });
	cb.SetTable (&table);
	return true;
}

bool VkEffect::CompilePass (Pass &p)
{
	VkMacros m = macros;
	m.push_back ({ "VS_ARGS", p.vsArgs.empty() ? std::string() : ", " + p.vsArgs }); // uniform arguments of "compile vs_3_0 Name(args)"
	m.push_back ({ "PS_ARGS", p.psArgs.empty() ? std::string() : ", " + p.psArgs });
	std::vector<uint32_t> vs, ps;
	VkConstTable tv, tp;
	if (!VkCompileGLSL (file.c_str(), src, p.vs.c_str(), VK_SHADER_STAGE_VERTEX_BIT, m, vs, &tv)) return false;
	if (!VkCompileGLSL (file.c_str(), src, p.ps.c_str(), VK_SHADER_STAGE_FRAGMENT_BIT, m, ps, &tp)) return false;
	p.vsObj = VkCreateShaderObject (dev, vs, VK_SHADER_STAGE_VERTEX_BIT);
	p.psObj = VkCreateShaderObject (dev, ps, VK_SHADER_STAGE_FRAGMENT_BIT);
	p.samplers.clear ();
	for (VkConstTable *t : { &tv, &tp })
		for (auto &e : t->entries) {
			if (!e.sampler) continue;
			bool have = false;
			for (auto &s : p.samplers) if (s.binding == e.binding) have = true;
			if (!have) p.samplers.push_back ({ e.binding, e.view });
		}
	return p.vsObj && p.psObj;
}

// pass states (D3DRS_* names as the effect files write them)

static bool StateBool (const std::string &v)
{
	return v == "true" || strtol (v.c_str(), NULL, 0) != 0;
}

static VkBlendFactor BlendFactor (const std::string &v)
{
	if (v == "zero") return VK_BLEND_FACTOR_ZERO;
	if (v == "one") return VK_BLEND_FACTOR_ONE;
	if (v == "srccolor") return VK_BLEND_FACTOR_SRC_COLOR;
	if (v == "invsrccolor") return VK_BLEND_FACTOR_ONE_MINUS_SRC_COLOR;
	if (v == "srcalpha") return VK_BLEND_FACTOR_SRC_ALPHA;
	if (v == "invsrcalpha") return VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA;
	if (v == "destalpha") return VK_BLEND_FACTOR_DST_ALPHA;
	if (v == "invdestalpha") return VK_BLEND_FACTOR_ONE_MINUS_DST_ALPHA;
	if (v == "destcolor") return VK_BLEND_FACTOR_DST_COLOR;
	if (v == "invdestcolor") return VK_BLEND_FACTOR_ONE_MINUS_DST_COLOR;
	if (v == "srcalphasat") return VK_BLEND_FACTOR_SRC_ALPHA_SATURATE;
	if (v == "blendfactor") return VK_BLEND_FACTOR_CONSTANT_COLOR;
	if (v == "invblendfactor") return VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_COLOR;
	LogWrn("VkEffect: blend factor %s not handled", v.c_str());
	return VK_BLEND_FACTOR_ONE;
}

static VkBlendOp BlendOp (const std::string &v)
{
	if (v == "subtract") return VK_BLEND_OP_SUBTRACT;
	if (v == "revsubtract") return VK_BLEND_OP_REVERSE_SUBTRACT;
	if (v == "min") return VK_BLEND_OP_MIN;
	if (v == "max") return VK_BLEND_OP_MAX;
	return VK_BLEND_OP_ADD;
}

static VkCompareOp CmpFunc (const std::string &v)
{
	if (v == "never") return VK_COMPARE_OP_NEVER;
	if (v == "less") return VK_COMPARE_OP_LESS;
	if (v == "equal") return VK_COMPARE_OP_EQUAL;
	if (v == "lessequal") return VK_COMPARE_OP_LESS_OR_EQUAL;
	if (v == "greater") return VK_COMPARE_OP_GREATER;
	if (v == "notequal") return VK_COMPARE_OP_NOT_EQUAL;
	if (v == "greaterequal") return VK_COMPARE_OP_GREATER_OR_EQUAL;
	return VK_COMPARE_OP_ALWAYS;
}

static VkStencilOp StencilOp (const std::string &v)
{
	if (v == "zero") return VK_STENCIL_OP_ZERO;
	if (v == "replace") return VK_STENCIL_OP_REPLACE;
	if (v == "incrsat") return VK_STENCIL_OP_INCREMENT_AND_CLAMP;
	if (v == "decrsat") return VK_STENCIL_OP_DECREMENT_AND_CLAMP;
	if (v == "invert") return VK_STENCIL_OP_INVERT;
	if (v == "incr") return VK_STENCIL_OP_INCREMENT_AND_WRAP;
	if (v == "decr") return VK_STENCIL_OP_DECREMENT_AND_WRAP;
	return VK_STENCIL_OP_KEEP;
}

void VkEffect::ApplyStates (const Pass &p)
{
	VkDev::State s = dev->GetState ();
	for (auto &kv : p.states) {
		std::string k = Lower (kv.first), v = Lower (kv.second);
		if (k == "zenable") s.depthTest = StateBool (v);
		else if (k == "zwriteenable") s.depthWrite = StateBool (v);
		else if (k == "zfunc") s.depthFunc = CmpFunc (v);
		else if (k == "alphablendenable") s.blend = StateBool (v);
		else if (k == "srcblend") s.blendEq.srcColorBlendFactor = BlendFactor (v);
		else if (k == "destblend") s.blendEq.dstColorBlendFactor = BlendFactor (v);
		else if (k == "blendop") s.blendEq.colorBlendOp = BlendOp (v);
		else if (k == "separatealphablendenable") s.blendSeparate = StateBool (v);
		else if (k == "srcblendalpha") s.blendEq.srcAlphaBlendFactor = BlendFactor (v);
		else if (k == "destblendalpha") s.blendEq.dstAlphaBlendFactor = BlendFactor (v);
		else if (k == "blendopalpha") s.blendEq.alphaBlendOp = BlendOp (v);
		else if (k == "cullmode") s.cull = (v == "none") ? VK_CULL_MODE_NONE : (v == "cw" ? VK_CULL_MODE_FRONT_BIT : VK_CULL_MODE_BACK_BIT);
		else if (k == "fillmode") s.fill = (v == "wireframe") ? VK_POLYGON_MODE_LINE : (v == "point" ? VK_POLYGON_MODE_POINT : VK_POLYGON_MODE_FILL);
		else if (k == "colorwriteenable") s.writeMask = (VkColorComponentFlags)(strtoul (v.c_str(), NULL, 0) & 0xF);
		else if (k == "stencilenable") s.stencil = StateBool (v);
		else if (k == "stencilfunc") s.stencilOp = CmpFunc (v);
		else if (k == "stencilref") s.stencilRef = (UINT)strtoul (v.c_str(), NULL, 0);
		else if (k == "stencilmask") s.stencilMask = (UINT)strtoul (v.c_str(), NULL, 0);
		else if (k == "stencilpass") s.stencilPass = StencilOp (v);
		else if (k == "stencilfail") s.stencilFail = StencilOp (v);
		else if (k == "stencilzfail") s.stencilZFail = StencilOp (v);
		else if (k == "pointspriteenable") {} // point sprites: the shaders read gl_PointCoord
		else LogWrn("VkEffect %s: pass state %s not handled", file.c_str(), kv.first.c_str());
	}
	if (!s.blendSeparate) { // D3D9: alpha blends like colour unless SEPARATEALPHABLENDENABLE is set
		s.blendEq.srcAlphaBlendFactor = s.blendEq.srcColorBlendFactor;
		s.blendEq.dstAlphaBlendFactor = s.blendEq.dstColorBlendFactor;
		s.blendEq.alphaBlendOp = s.blendEq.colorBlendOp;
	}
	dev->SetState (s);
}

// ID3DXEffect interface

VkFxHandle VkEffect::GetParameterByName (VkFxHandle parent, const char *name)
{
	if (parent) {
		std::string n = ((VkConstHandle)parent)->name + "." + name;
		return table.GetConstantByName (n.c_str());
	}
	return table.GetConstantByName (name);
}

VkFxHandle VkEffect::GetTechniqueByName (const char *name)
{
	for (auto &t : tech) if (t.name == name) return &t;
	return NULL; // D3DX returns NULL without a message (D3D9Client asks for techniques D3D9Client.fx no longer has)
}

int VkEffect::SetTechnique (VkFxHandle t)
{
	cur = (Technique *)t;
	return cur ? 0 : -1;
}

int VkEffect::Begin (UINT *passes, DWORD flags)
{
	if (!cur) return -1;
	saved = !(flags & VKFX_DONOTSAVESTATE);
	if (saved) savedState = dev->GetState ();
	if (passes) *passes = (UINT)cur->pass.size();
	return 0;
}

int VkEffect::BeginPass (UINT i)
{
	if (!cur || i >= cur->pass.size()) return -1;
	Pass &p = cur->pass[i];
	if (!p.vsObj && !p.failed) p.failed = !CompilePass (p);
	if (p.failed) return -1;
	ApplyStates (p);
	dev->BindShaders (p.vsObj, p.psObj);
	dev->SetConstantSource (&cb, &p.samplers);
	curPass = (int)i;
	return CommitChanges ();
}

int VkEffect::CommitChanges ()
{
	if (!cur || curPass < 0) return -1;
	cb.Invalidate (); // pushed with the next draw
	return 0;
}

int VkEffect::EndPass ()
{
	curPass = -1;
	return 0;
}

int VkEffect::End ()
{
	if (saved) dev->SetState (savedState);
	saved = false;
	return 0;
}

int VkEffect::SetBool (VkFxHandle h, BOOL b)
{
	uint32_t v = b ? 1 : 0; // GLSL bools in a block are 32-bit, like BOOL
	cb.SetValue ((VkConstHandle)h, &v, 4);
	return h ? 0 : -1;
}

int VkEffect::SetInt (VkFxHandle h, int i)
{
	cb.SetValue ((VkConstHandle)h, &i, 4);
	return h ? 0 : -1;
}

int VkEffect::SetFloat (VkFxHandle h, float f)
{
	cb.SetValue ((VkConstHandle)h, &f, 4);
	return h ? 0 : -1;
}

int VkEffect::SetVector (VkFxHandle h, const D3DXVECTOR4 *v)
{
	cb.SetValue ((VkConstHandle)h, v, 16); // SetValue stops at the parameter's size (float3 takes xyz)
	return h ? 0 : -1;
}

int VkEffect::SetMatrix (VkFxHandle h, const D3DXMATRIX *m)
{
	cb.SetValue ((VkConstHandle)h, m, 64); // row_major blocks: D3DX's layout as is
	return h ? 0 : -1;
}

int VkEffect::SetValue (VkFxHandle h, const void *data, UINT bytes)
{
	cb.SetValue ((VkConstHandle)h, data, bytes); // scalar blocks: tightly packed, as D3DX takes the data
	return h ? 0 : -1;
}

int VkEffect::GetSamplerState (VkFxHandle h, VkSamplerDesc *desc)
{
	VkConstHandle e = (VkConstHandle)h;
	if (!e || !e->sampler || e->binding < 0 || !desc) return -1;
	for (auto &s : samp) if (s.binding == e->binding) { *desc = s.desc; return 0; }
	return -1;
}
int VkEffect::SetSamplerState (VkFxHandle h, const VkSamplerDesc *desc)
{
	VkConstHandle e = (VkConstHandle)h;
	if (!e || !e->sampler || e->binding < 0) return -1;
	for (auto &s : samp) if (s.binding == e->binding) {
		s.desc = desc ? *desc : s.state;
		VkTex *t = cb.GetTexture (s.binding);
		if (t) cb.SetTexture (s.binding, t, s.desc); // the bound texture takes the new state at the next draw
		return 0;
	}
	return -1;
}
int VkEffect::SetTexture (VkFxHandle h, VkTex *t)
{
	VkConstHandle e = (VkConstHandle)h;
	if (!e || !e->sampler) return -1;
	if (e->binding < 0) { // texture parameter: every sampler_state that reads it
		for (auto &s : samp) if (s.texture == e->name) cb.SetTexture (s.binding, t, s.desc);
	}
	else {
		for (auto &s : samp) if (s.binding == e->binding) cb.SetTexture (s.binding, t, s.desc);
	}
	return 0;
}

int VkEffect::GetFloat (VkFxHandle h, float *f)
{
	cb.GetValue ((VkConstHandle)h, f, 4);
	return h ? 0 : -1;
}

int VkEffect::GetMatrix (VkFxHandle h, D3DXMATRIX *m)
{
	cb.GetValue ((VkConstHandle)h, m, 64);
	return h ? 0 : -1;
}

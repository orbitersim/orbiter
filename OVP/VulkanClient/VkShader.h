// not upstream: GLSL compilation at run time (D3DXCompileShader), constant tables (ID3DXConstantTable) and effects (ID3DXEffect)

#ifndef __VKSHADER_H
#define __VKSHADER_H

#include "VkCore.h"
#include "D3DXMath.h"
#include <string>
#include <vector>
#include <deque>
#include <map>

// shader constant or sampler (D3DXCONSTANT_DESC counterpart)
struct VkConstEntry {
	std::string name;
	int binding;           // uniform block binding, or the sampler's binding
	UINT offset;           // byte offset in the block (scalar layout, as the client's tightly packed structs)
	UINT size;             // bytes
	bool sampler;          // sampler, or an effect's texture parameter (binding -1)
	VkImageViewType view;  // sampler dimension
};

// sampler binding a shader pair reads
struct VkSamplerSlot {
	int binding;
	VkImageViewType view;
};

// constants and samplers of a compiled stage or effect, looked up by name
class VkConstTable {
public:
	VkConstHandle GetConstantByName (const char *name) const;
	void Merge (const VkConstTable &t);
	std::deque<VkConstEntry> entries;    // deque: handles stay valid as entries are added
	std::map<int, UINT> blocks;          // uniform block binding → size
};

// macros for a shader compile (D3DXMACRO)
typedef std::vector<std::pair<std::string, std::string>> VkMacros;

// reads a shader file with its #include files expanded
bool VkLoadShaderSource (const char *file, std::string &src);

// compiles the stage of a GLSL file whose entry is picked with "#define VS_<entry>" or "#define PS_<entry>"
bool VkCompileGLSL (const char *file, const std::string &src, const char *entry, VkShaderStageFlagBits stage,
	const VkMacros &macros, std::vector<uint32_t> &spirv, VkConstTable *table);

// shader object for a compiled stage (IDirect3DDevice9::CreateVertexShader/CreatePixelShader)
VkShaderEXT VkCreateShaderObject (VkDev *dev, const std::vector<uint32_t> &spirv, VkShaderStageFlagBits stage);

// uniform block contents and bound textures of a shader pair, pushed per draw
class VkConstBuffer {
public:
	explicit VkConstBuffer (VkDev *dev);
	~VkConstBuffer ();
	VkConstBuffer (const VkConstBuffer&) = delete;
	VkConstBuffer& operator= (const VkConstBuffer&) = delete;
	void SetTable (const VkConstTable *t);
	void SetValue (VkConstHandle h, const void *data, UINT bytes);
	void GetValue (VkConstHandle h, void *data, UINT bytes) const;
	void SetTexture (int binding, VkTex *tex, const VkSamplerDesc &s);
	void ClearTextures ();
	void Push (const std::vector<VkSamplerSlot> &samplers); // pushes the blocks and the listed sampler bindings
	VkTex *GetTexture (int binding) const;
	void DropTexture (const VkTex *t);                 // VkDev::ForgetTexture, under ConstLock
	void Invalidate () { dirty = true; }
	bool IsDirty () const { return dirty; }
private:
	VkDev *dev;
	bool dirty;
	std::map<int, std::vector<BYTE>> data;             // block binding → contents
	struct Tex { VkTex *tex; VkSamplerDesc s; };
	std::map<int, Tex> tex;
};

// effect parameter or technique (D3DXHANDLE)
typedef const void *VkFxHandle;

#define VKFX_DONOTSAVESTATE 0x1 // D3DXFX_DONOTSAVESTATE

// effect file: GLSL with D3DX effect declarations (texture, sampler_state, technique/pass) handled here
class VkEffect {
public:
	static VkEffect *Create (VkDev *dev, const char *file, const VkMacros &macros); // D3DXCreateEffectFromFile; NULL on error
	~VkEffect ();

	VkFxHandle GetParameterByName (VkFxHandle parent, const char *name);
	VkFxHandle GetTechniqueByName (const char *name);

	int SetTechnique (VkFxHandle tech);
	int Begin (UINT *passes, DWORD flags);
	int BeginPass (UINT pass);
	int CommitChanges ();
	int EndPass ();
	int End ();

	int SetBool (VkFxHandle h, BOOL b);
	int SetInt (VkFxHandle h, int i);
	int SetFloat (VkFxHandle h, float f);
	int SetVector (VkFxHandle h, const D3DXVECTOR4 *v);
	int SetMatrix (VkFxHandle h, const D3DXMATRIX *m);
	int SetValue (VkFxHandle h, const void *data, UINT bytes);
	int SetTexture (VkFxHandle h, VkTex *tex);
	int GetSamplerState (VkFxHandle sampler, VkSamplerDesc *desc);       // the sampler's current state
	int SetSamplerState (VkFxHandle sampler, const VkSamplerDesc *desc); // SetSamplerState on an effect sampler; NULL restores its sampler_state
	int GetFloat (VkFxHandle h, float *f);
	int GetMatrix (VkFxHandle h, D3DXMATRIX *m);

	struct Pass {
		std::string vs, ps, vsArgs, psArgs;  // entry functions and their uniform arguments
		std::vector<std::pair<std::string, std::string>> states;
		VkShaderEXT vsObj, psObj;
		bool failed;
		std::vector<VkSamplerSlot> samplers; // sampler bindings the pass reads
	};
	struct Technique {
		std::string name;
		std::vector<Pass> pass;
	};
	struct Sampler {
		std::string name, texture;
		int binding;
		VkImageViewType view;
		VkSamplerDesc desc;
		VkSamplerDesc state;               // as declared in sampler_state
	};

private:
	VkEffect (VkDev *dev);
	bool Parse (const char *file, const VkMacros &macros);
	bool CompilePass (Pass &p);
	void ParseTechnique (const std::string &code, size_t &i);
	void ApplyStates (const Pass &p);

	VkDev *dev;
	std::string file, src;               // GLSL with the effect declarations removed
	VkMacros macros;
	std::vector<Technique> tech;
	std::vector<Sampler> samp;
	std::vector<std::string> textures;   // texture parameters
	VkConstTable table;
	VkConstBuffer cb;
	Technique *cur;
	int curPass;
	bool saved;
	VkDev::State savedState;
};

#endif // !__VKSHADER_H

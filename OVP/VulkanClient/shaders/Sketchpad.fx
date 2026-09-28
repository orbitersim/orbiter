// =================================================================================================================================
// The MIT Lisence:
//
// Copyright (C) 2013-2016 Jarmo Nikkanen
//
// Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation
// files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy,
// modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software
// is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
// OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
// LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR
// IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
// =================================================================================================================================



// ----------------------------------------------------------------------------
// Sketchpad Implementation
// ----------------------------------------------------------------------------

layout(binding = 0, row_major, scalar) uniform FxTransforms	// the uniform extern parameters before the textures
{
mat4	 gColorMatrix;
mat4     gVP;			    // Projection matrix
mat4     gW;			    // World matrix
mat4     gWVP;				// World View Projection
};

// Textures
uniform extern texture   gFnt0;
uniform extern texture   gTex0;			    // Diffuse texture
uniform extern texture	 gNoiseTex;

layout(binding = 1, row_major, scalar) uniform FxParams	// the uniform extern parameters after the textures
{
// Colors
vec4     gPen;
vec4     gKey;
vec4     gMtrl;
vec4     gNoiseColor;
vec4     gGamma;

vec3     gPos;				// Clipper sphere direction [unit vector]
vec3     gPos2;				// Clipper cone direction [unit vector]
vec4     gCov;				// Clipper sphere coverage parameters
vec4     gSize;				// Inverse Texture size in .xy [pixels]
vec4     gTarget;			// Inverse Screen size in .xy [pixels], Screen Size in .zw [pixels]
vec3	 gWidth;			// Pen width in .x, and pattern scale in .y, pixel offset in .z
float	 gFov;				// atan( 2 * tan(fov/2) / H )
float	 gRandom;
bool     gDashEn;
bool     gTexEn;
bool	 gFntEn;
bool     gKeyEn;
bool     gWide;				// Unused
bool     gShade;
bool     gClipEn;
bool	 gClearEn;			// Unused
bool	 gEffectsEn;
};


// ColorKey tolarance
#define tol 0.01f

sampler2D TexS = sampler_state	// register (s0) left out: VkEffect picks the binding, D3D9Pad finds it by name
{
	Texture = <gTex0>;
	MinFilter = LINEAR;
	MagFilter = LINEAR;
	MipFilter = LINEAR;
	MaxAnisotropy = 8;
	AddressU = CLAMP;
	AddressV = CLAMP;
};

sampler2D FntS = sampler_state	// register (s1) left out
{
	Texture = <gFnt0>;
	MinFilter = LINEAR;
	MagFilter = LINEAR;
	MipFilter = LINEAR;
	MaxAnisotropy = 8;
	AddressU = CLAMP;
	AddressV = CLAMP;
};

sampler2D NoiseS = sampler_state	// register (s2) left out
{
	Texture = <gNoiseTex>;
	MinFilter = LINEAR;
	MagFilter = LINEAR;
	MipFilter = LINEAR;
	AddressU = WRAP;
	AddressV = WRAP;
};


struct InputVS
{
	vec3 pos;     // POSITION0, vertex x, y
	vec4 dir;     // TEXCOORD0, Texture coord or inbound direction
	vec4 clr;     // COLOR0, Color
	vec4 fnc;     // COLOR1, Function switch
};

#define SSW 3	// Point side switch
#define TSW 2	// Fragment, Pen, Texture switch
#define CSW 1   // ColorKey, Font switch
#define LSW 0   // Length switch

struct OutputVS
{
	vec4 posH;     // POSITION0
	vec4 sw;       // TEXCOORD0
	vec4 tex;      // TEXCOORD1
	float  len;    // TEXCOORD2
	vec4 posW;     // TEXCOORD3
	vec4 color;    // COLOR0
};

struct SkpMshVS
{
	vec4 posH;     // POSITION0
	vec3 nrmW;     // TEXCOORD0
	vec2 tex;      // TEXCOORD1
};

struct NTVERTEX {                        // D3D9Client Mesh vertex layout
	vec3 posL;     // POSITION0
	vec3 nrmL;     // NORMAL0
	vec2 tex0;     // TEXCOORD0
};


float cmax(vec3 v)
{
	return max(v.x, max(v.y, v.z));
}

// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------

OutputVS Sketch3DVS(InputVS v)
{
	// Zero output.
	OutputVS outVS = OutputVS(vec4(0), vec4(0), vec4(0), 0.0, vec4(0), vec4(0));

	vec3 posW = (vec4(v.pos.xy, 0.0f, 1.0f) * gW).xyz;
	vec3 prvW = (vec4(v.dir.zw, 0.0f, 1.0f) * gW).xyz;
	vec3 nxtW = (vec4(v.dir.xy, 0.0f, 1.0f) * gW).xyz;

	outVS.len = v.pos.z;

	vec3 posN = normalize(posW);
	vec3 prvN = normalize(prvW);
	vec3 nxtN = normalize(nxtW);
	vec3 nxtS = normalize(cross(nxtN - posN, posN));
	vec3 prvS = normalize(cross(posN - prvN, posN));
	vec3 latN = normalize(nxtS + prvS) * (0.45*gWidth.x) * inversesqrt(max(0.1, 0.5f + dot(nxtS, prvS)*0.5f));

	if (v.fnc[LSW]>0.5f) outVS.len = min(1.0, length(posN-prvN)) / gFov;
	else				 outVS.len = v.pos.z;

	float fSide = round(v.fnc[SSW] * 2.0 - 1.0);
	float fPosD = dot(posN, posW);

	if (fSide != 0.0) posW += latN * (fSide * fPosD * gFov); // not upstream: D3D9 multiplies NaN by 0 to 0, GLSL keeps the NaN of a zero-length normalize
	
	outVS.color.rgba = v.clr.bgra;
	outVS.posW = vec4(posW, fPosD);
	outVS.sw = v.fnc;
	outVS.posH = vec4(posW.xyz, 1.0f) * gVP;
	outVS.tex = vec4(v.dir.xy * gSize.xy, v.dir.xy * gSize.zw);

	return outVS;
}

#if defined(VS_Sketch3DVS) || defined(VS_OrthoVS)
layout(location = 0) in vec3 iPos;
layout(location = 5) in vec4 iDir;
layout(location = 3) in vec4 iClr;
layout(location = 4) in vec4 iFnc;
layout(location = 0) out OutputVS oVS;
#endif
#ifdef VS_Sketch3DVS
void main() { oVS = Sketch3DVS(InputVS(iPos, iDir, iClr, iFnc) VS_ARGS); gl_Position = oVS.posH; }
#endif



// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------

OutputVS OrthoVS(InputVS v)
{
	// Zero output.
	OutputVS outVS = OutputVS(vec4(0), vec4(0), vec4(0), 0.0, vec4(0), vec4(0));

	vec4 posH = vec4(v.pos.xy, 0.0f, 1.0f) * gWVP;
	vec4 prvH = vec4(v.dir.zw, 0.0f, 1.0f) * gWVP;

	if (v.fnc[LSW]>0.5f) outVS.len = length((posH.xy - prvH.xy) * gTarget.zw * 0.5);
	else				 outVS.len = v.pos.z;

	outVS.posW = vec4(0);

	if (gWide) {

		vec4 nxtH = vec4(v.dir.xy, 0.0f, 1.0f) * gWVP;
		float fSide = round(v.fnc[SSW] * 2.0 - 1.0);
		vec2 pixH = gTarget.xy * gWidth.z * abs(fSide);

		nxtH.xy -= pixH;
		posH.xy -= pixH;
		prvH.xy -= pixH;

		vec2 nxtS = normalize(nxtH.xy - posH.xy);
		vec2 prvS = normalize(posH.xy - prvH.xy);
		vec2 latW = normalize(nxtS + prvS) * (0.45*gWidth.x) * inversesqrt(max(0.1, 0.5f + dot(nxtS, prvS)*0.5f));

		if (fSide != 0.0) posH += vec4(latW.y, -latW.x, 0, 0) * gTarget * fSide; // not upstream: D3D9 multiplies NaN by 0 to 0, GLSL keeps the NaN of a zero-length normalize
	}

	// not upstream: pen vertices lie on D3D9's integer pixel centres, Vulkan's are at .5 (the fills dropped their -0.5 instead)
	if (!gWide || abs(round(v.fnc[SSW] * 2.0 - 1.0)) > 0.5) posH.xy += vec2(gTarget.x, -gTarget.y) * 0.5;
	
	outVS.color.rgba = v.clr.bgra;
	outVS.sw = v.fnc;
	outVS.posH = vec4(posH.xyz, 1.0f);

	outVS.tex = vec4(v.dir.xy * gSize.xy, v.dir.xy * gSize.zw);

	return outVS;
}

#ifdef VS_OrthoVS
void main() { oVS = OrthoVS(InputVS(iPos, iDir, iClr, iFnc) VS_ARGS); gl_Position = oVS.posH; }
#endif





// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------

#ifdef STAGE_PS
vec4 SketchpadPS(vec4 sc, OutputVS frg)	// sc : VPOS
{
	vec4 t = vec4(1);
	vec3 u = vec3(1);

	if (gFntEn)	u = texture(FntS, frg.tex.zw).rgb;
	if (gTexEn) t = texture(TexS, frg.tex.xy);	

	float f = max(u.r*0.7f, u.g);

	// Select Color source
	vec4 c = frg.color;
	if (frg.sw[TSW] > 0.2f) c = gPen;
	if (frg.sw[TSW] > 0.8f) c = t;
	if (frg.sw[CSW] > 0.8f) c.a *= f;

	// Color keying
	if (gTexEn && gKeyEn) {
		vec4 x = abs(c - gKey);
		if ((x.r < tol) && (x.g < tol) && (x.b < tol)) {
			if (frg.sw[CSW] > 0.2f && frg.sw[CSW] < 0.8) discard; // clip(-1)
		}
	}
	
	if (gDashEn) {
		float q;
		if (modf(frg.len*gWidth.y, q) > 0.5f) discard; // clip(-1)
	}

	if (gClipEn) {
		vec3 posN = normalize(frg.posW.xyz);
		if ((dot(gPos,  posN) > gCov.x) && (frg.posW.w > gCov.y)) discard; // clip(-1)
		if ((dot(gPos2, posN) > gCov.z) && (frg.posW.w > gCov.w)) discard; // clip(-1)
	}

	if (gEffectsEn) {

		// Apply color matrix
		c = c * gColorMatrix;
		
		// Apply gamma correction
		c.rgb = pow(max(c.rgb, vec3(0)), gGamma.rgb);

		// Color overboost correction beyond 0-1 range
		c.rgb += clamp(cmax(c.rgb) - 1.0f, 0.0, 1.0);

		// Apply noise
		float noise = (texture(NoiseS, sc.xy*(1.0f / 128.0f) + vec2(gRandom, gRandom*7.0)).r * 2.0f) - 1.0f;
		c.rgb += mix(vec3(1, 1, 1), c.rgb, gNoiseColor.a) * gNoiseColor.rgb * noise;	
	}

	return clamp(c, 0.0, 1.0);
}
#endif

#ifdef PS_SketchpadPS
layout(location = 0) in OutputVS frg;
layout(location = 0) out vec4 oColor;
void main() { oColor = SketchpadPS(vec4(gl_FragCoord.xy - 0.5, 0, 0), frg PS_ARGS); } // VPOS
#endif


technique SketchTech
{
	pass P0
	{
		vertexShader = compile vs_3_0 OrthoVS();
		pixelShader  = compile ps_3_0 SketchpadPS();
		AlphaBlendEnable = true;
		BlendOp = Add;
		SrcBlend = SrcAlpha;
		DestBlend = InvSrcAlpha;
		ZEnable = false;
		ZWriteEnable = false;
		CullMode = None;
	}

	pass P1
	{
		vertexShader = compile vs_3_0 Sketch3DVS();
		pixelShader = compile ps_3_0 SketchpadPS();
		AlphaBlendEnable = true;
		BlendOp = Add;
		SrcBlend = SrcAlpha;
		DestBlend = InvSrcAlpha;
		ZEnable = false;
		ZWriteEnable = false;
		CullMode = None;
	}
}



// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------

SkpMshVS SketchMeshVS(NTVERTEX v)
{
	// Zero output.
	SkpMshVS outVS = SkpMshVS(vec4(0), vec3(0), vec2(0));
	vec3 posW = (vec4(v.posL, 1.0f) * gW).xyz;
	vec3 nrmW = (vec4(v.nrmL, 0.0f) * gW).xyz;
	outVS.posH  = vec4(posW, 1.0f) * gVP;
	outVS.tex = v.tex0;
	outVS.nrmW = nrmW;
	return outVS;

}

#ifdef VS_SketchMeshVS
layout(location = 0) in vec3 iPosL;
layout(location = 1) in vec3 iNrmL;
layout(location = 5) in vec2 iTex0;
layout(location = 0) out SkpMshVS oVS;
void main() { oVS = SketchMeshVS(NTVERTEX(iPosL, iNrmL, iTex0) VS_ARGS); gl_Position = oVS.posH; }
#endif


vec4 SketchMeshPS(SkpMshVS frg)
{
	vec4 cTex = vec4(1);
	float fS = 1;
	if (gTexEn) cTex = texture(TexS, frg.tex);
	if (gShade) fS = dot(normalize(frg.nrmW), vec3(0, 0, -1));

	cTex.rgba *= gPen.bgra;
	cTex.rgba *= gMtrl.rgba;
	cTex.rgb  *= clamp(fS, 0.0, 1.0);

	return cTex;
}

#ifdef PS_SketchMeshPS
layout(location = 0) in SkpMshVS frg;
layout(location = 0) out vec4 oColor;
void main() { oColor = SketchMeshPS(frg PS_ARGS); }
#endif


technique SketchMesh
{
	pass P0
	{
		vertexShader = compile vs_3_0 SketchMeshVS();
		pixelShader = compile ps_3_0 SketchMeshPS();
		AlphaBlendEnable = true;
		BlendOp = Add;
		SrcBlend = SrcAlpha;
		DestBlend = InvSrcAlpha;
		ZEnable = false;
		ZWriteEnable = false;
	}
}
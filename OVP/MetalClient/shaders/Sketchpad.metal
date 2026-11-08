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

// Metal port of Sketchpad.fx

#include <metal_stdlib>
using namespace metal;

struct Globals
{
	float4x4  gColorMatrix;
	float4x4  gVP;
	float4x4  gW;
	float4x4  gWVP;
	float4    gPen;
	float4    gKey;
	float4    gMtrl;
	float4    gNoiseColor;
	float4    gGamma;
	float3    gPos;
	float3    gPos2;
	float4    gCov;
	float4    gSize;
	float4    gTarget;
	float3    gWidth;
	float     gFov;
	float     gRandom;
	int       gDashEn;
	int       gTexEn;
	int       gFntEn;
	int       gKeyEn;
	int       gWide;
	int       gShade;
	int       gClipEn;
	int       gClearEn;
	int       gEffectsEn;
};

// Textures: gTex0 0, gFnt0 1, gNoiseTex 2


#define tol 0.01f

constexpr sampler TexS(filter::linear, mip_filter::linear, address::clamp_to_edge);
constexpr sampler FntS(filter::linear, mip_filter::linear, address::clamp_to_edge);
constexpr sampler NoiseS(filter::linear, mip_filter::linear, address::repeat);


struct InputVS
{
	float3 pos [[attribute(0)]];
	float4 dir [[attribute(4)]];
	float4 clr [[attribute(12)]];
	float4 fnc [[attribute(13)]];
};

#define SSW 3
#define TSW 2
#define CSW 1
#define LSW 0

struct OutputVS
{
	float4 posH    [[position]];
	float4 sw;
	float4 tex;
	float  len;
	float4 posW;
	float4 color;
};

struct SkpMshVS
{
	float4 posH    [[position]];
	float3 nrmW;
	float2 tex;
};

struct NTVERTEX {
	float3 posL   [[attribute(0)]];
	float3 nrmL   [[attribute(2)]];
	float2 tex0   [[attribute(4)]];
};


float cmax(float3 v)
{
	return max(v.x, max(v.y, v.z));
}

// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------

vertex OutputVS Sketch3DVS(InputVS v [[stage_in]], constant Globals &g [[buffer(0)]])
{
	OutputVS outVS = {};

	float3 posW = (g.gW * float4(v.pos.xy, 0.0f, 1.0f)).xyz;
	float3 prvW = (g.gW * float4(v.dir.zw, 0.0f, 1.0f)).xyz;
	float3 nxtW = (g.gW * float4(v.dir.xy, 0.0f, 1.0f)).xyz;

	outVS.len = v.pos.z;

	float3 posN = normalize(posW);
	float3 prvN = normalize(prvW);
	float3 nxtN = normalize(nxtW);
	float3 nxtS = normalize(cross(nxtN - posN, posN));
	float3 prvS = normalize(cross(posN - prvN, posN));
	float3 latN = normalize(nxtS + prvS) * (0.45f*g.gWidth.x) * rsqrt(max(0.1f, 0.5f + dot(nxtS, prvS)*0.5f));

	if (v.fnc[LSW]>0.5f) outVS.len = min(1.0f, length(posN-prvN)) / g.gFov;
	else				 outVS.len = v.pos.z;

	float fSide = round(v.fnc[SSW] * 2.0f - 1.0f);
	float fPosD = dot(posN, posW);

	if (fSide != 0.0f) posW += latN * (fSide * fPosD * g.gFov);

	outVS.color.rgba = v.clr.bgra;
	outVS.posW = float4(posW, fPosD);
	outVS.sw = v.fnc;
	outVS.posH = g.gVP * float4(posW.xyz, 1.0f);
	outVS.tex = float4(v.dir.xy * g.gSize.xy, v.dir.xy * g.gSize.zw);

	return outVS;
}



// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------

vertex OutputVS OrthoVS(InputVS v [[stage_in]], constant Globals &g [[buffer(0)]])
{
	OutputVS outVS = {};

	float4 posH = g.gWVP * float4(v.pos.xy, 0.0f, 1.0f);
	float4 prvH = g.gWVP * float4(v.dir.zw, 0.0f, 1.0f);

	if (v.fnc[LSW]>0.5f) outVS.len = length((posH.xy - prvH.xy) * g.gTarget.zw * 0.5f);
	else				 outVS.len = v.pos.z;

	outVS.posW = 0.0f;

	float fSide = round(v.fnc[SSW] * 2.0f - 1.0f);

	if (g.gWide && fSide != 0.0f) {

		float4 nxtH = g.gWVP * float4(v.dir.xy, 0.0f, 1.0f);
		float2 pixH = g.gTarget.xy * g.gWidth.z * abs(fSide);

		nxtH.xy -= pixH;
		posH.xy -= pixH;
		prvH.xy -= pixH;

		float2 nxtS = normalize(nxtH.xy - posH.xy);
		float2 prvS = normalize(posH.xy - prvH.xy);
		float2 latW = normalize(nxtS + prvS) * (0.45f*g.gWidth.x) * rsqrt(max(0.1f, 0.5f + dot(nxtS, prvS)*0.5f));

		posH += float4(latW.y, -latW.x, 0.0f, 0.0f) * g.gTarget * fSide;
	}

	outVS.color.rgba = v.clr.bgra;
	outVS.sw = v.fnc;
	outVS.posH = float4(posH.xyz, 1.0f);

	outVS.tex = float4(v.dir.xy * g.gSize.xy, v.dir.xy * g.gSize.zw);

	return outVS;
}





// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------

fragment float4 SketchpadPS(OutputVS frg [[stage_in]], constant Globals &g [[buffer(0)]],
	texture2d<float> gTex0 [[texture(0)]],
	texture2d<float> gFnt0 [[texture(1)]],
	texture2d<float> gNoiseTex [[texture(2)]])
{
	float4 sc = float4(floor(frg.posH.xy), 0.0f, 0.0f);
	float4 t = 1.0f;
	float3 u = 1.0f;

	if (g.gFntEn)	u = gFnt0.sample(FntS, frg.tex.zw).rgb;
	if (g.gTexEn) t = gTex0.sample(TexS, frg.tex.xy);

	float f = max(u.r*0.7f, u.g);

	float4 c = frg.color;
	if (frg.sw[TSW] > 0.2f) c = g.gPen;
	if (frg.sw[TSW] > 0.8f) c = t;
	if (frg.sw[CSW] > 0.8f) c.a *= f;

	if (g.gTexEn && g.gKeyEn) {
		float4 x = abs(c - g.gKey);
		if ((x.r < tol) && (x.g < tol) && (x.b < tol)) {
			if (frg.sw[CSW] > 0.2f && frg.sw[CSW] < 0.8f) discard_fragment();
		}
	}

	if (g.gDashEn) {
		float q;
		if (modf(frg.len*g.gWidth.y, q) > 0.5f) discard_fragment();
	}

	if (g.gClipEn) {
		float3 posN = normalize(frg.posW.xyz);
		if ((dot(g.gPos,  posN) > g.gCov.x) && (frg.posW.w > g.gCov.y)) discard_fragment();
		if ((dot(g.gPos2, posN) > g.gCov.z) && (frg.posW.w > g.gCov.w)) discard_fragment();
	}

	if (g.gEffectsEn) {

		c = g.gColorMatrix * c;

		c.rgb = pow(max(c.rgb, float3(0.0f)), g.gGamma.rgb);

		c.rgb += saturate(cmax(c.rgb) - 1.0f);

		float noise = (gNoiseTex.sample(NoiseS, sc.xy*(1.0f / 128.0f) + float2(g.gRandom, g.gRandom*7.0f)).r * 2.0f) - 1.0f;
		c.rgb += mix(float3(1.0f, 1.0f, 1.0f), c.rgb, g.gNoiseColor.a) * g.gNoiseColor.rgb * noise;
	}

	return saturate(c);
}


// @technique SketchTech
// @pass P0 vs=OrthoVS ps=SketchpadPS AlphaBlendEnable=true BlendOp=Add SrcBlend=SrcAlpha DestBlend=InvSrcAlpha ZEnable=false ZWriteEnable=false CullMode=None
// @pass P1 vs=Sketch3DVS ps=SketchpadPS AlphaBlendEnable=true BlendOp=Add SrcBlend=SrcAlpha DestBlend=InvSrcAlpha ZEnable=false ZWriteEnable=false CullMode=None



// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------

vertex SkpMshVS SketchMeshVS(NTVERTEX v [[stage_in]], constant Globals &g [[buffer(0)]])
{
	SkpMshVS outVS = {};
	float3 posW = (g.gW * float4(v.posL, 1.0f)).xyz;
	float3 nrmW = (g.gW * float4(v.nrmL, 0.0f)).xyz;
	outVS.posH  = g.gVP * float4(posW, 1.0f);
	outVS.tex = v.tex0;
	outVS.nrmW = nrmW;
	return outVS;

}


fragment float4 SketchMeshPS(SkpMshVS frg [[stage_in]], constant Globals &g [[buffer(0)]], texture2d<float> gTex0 [[texture(0)]])
{
	float4 cTex = 1.0f;
	float fS = 1.0f;
	if (g.gTexEn) cTex = gTex0.sample(TexS, frg.tex);
	if (g.gShade) fS = dot(normalize(frg.nrmW), float3(0.0f, 0.0f, -1.0f));

	cTex.rgba *= g.gPen.bgra;
	cTex.rgba *= g.gMtrl.rgba;
	cTex.rgb  *= saturate(fS);

	return cTex;
}


// @technique SketchMesh
// @pass P0 vs=SketchMeshVS ps=SketchMeshPS AlphaBlendEnable=true BlendOp=Add SrcBlend=SrcAlpha DestBlend=InvSrcAlpha ZEnable=false ZWriteEnable=false

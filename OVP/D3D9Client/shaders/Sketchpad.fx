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

uniform extern float4x4	 gColorMatrix;
uniform extern float4x4  gVP;			    // Projection matrix
uniform extern float4x4  gW;			    // World matrix
uniform extern float4x4  gWVP;				// World View Projection

// Textures
uniform extern texture   gFnt0;
uniform extern texture   gTex0;			    // Diffuse texture
uniform extern texture	 gNoiseTex;

// Colors
uniform extern float4    gPen;
uniform extern float4    gKey;
uniform extern float4    gMtrl;
uniform extern float4    gNoiseColor;
uniform extern float4    gGamma;

uniform extern float3    gPos;				// Clipper sphere direction [unit vector]
uniform extern float3    gPos2;				// Clipper cone direction [unit vector]
uniform extern float4    gCov;				// Clipper sphere coverage parameters
uniform extern float4    gSize;				// Inverse Texture size in .xy [pixels]
uniform extern float4    gTarget;			// Inverse Screen size in .xy [pixels], Screen Size in .zw [pixels]
uniform extern float3	 gWidth;			// Pen width in .x, and pattern scale in .y, pixel offset in .z
uniform extern float	 gFov;				// atan( 2 * tan(fov/2) / H )
uniform extern float	 gRandom;
uniform extern bool      gDashEn;
uniform extern bool      gTexEn;
uniform extern bool		 gFntEn;
uniform extern bool      gKeyEn;
uniform extern bool      gWide;				// Unused
uniform extern bool      gShade;
uniform extern bool      gClipEn;
uniform extern bool		 gClearEn;			// Unused
uniform extern bool		 gEffectsEn;


// ColorKey tolarance
#define tol 0.01f

sampler TexS : register(s0) = sampler_state
{
	Texture = <gTex0>;
	MinFilter = LINEAR;
	MagFilter = LINEAR;
	MipFilter = LINEAR;
	MaxAnisotropy = 8;
	AddressU = CLAMP;
	AddressV = CLAMP;
};

sampler FntS : register(s1) = sampler_state
{
	Texture = <gFnt0>;
	MinFilter = LINEAR;
	MagFilter = LINEAR;
	MipFilter = LINEAR;
	MaxAnisotropy = 8;
	AddressU = CLAMP;
	AddressV = CLAMP;
};

sampler NoiseS : register(s2) = sampler_state
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
	float3 pos : POSITION0;				// vertex x, y
	float4 dir : TEXCOORD0;				// Texture coord or inbound direction
	float4 clr : COLOR0;				// Color
	float4 fnc : COLOR1;				// Function switch
};

#define SSW 3	// Point side switch
#define TSW 2	// Fragment, Pen, Texture switch
#define CSW 1   // ColorKey, Font switch
#define LSW 0   // Length switch

struct OutputVS
{
	float4 posH    : POSITION0;
	float4 sw      : TEXCOORD0;
	float4 tex     : TEXCOORD1;
	float  len	   : TEXCOORD2;
	float4 posW    : TEXCOORD3;
	float4 color   : COLOR0;
};

struct SkpMshVS
{
	float4 posH    : POSITION0;
	float3 nrmW    : TEXCOORD0;
	float2 tex     : TEXCOORD1;
};

struct NTVERTEX {                        // D3D9Client Mesh vertex layout
	float3 posL   : POSITION0;
	float3 nrmL   : NORMAL0;
	float2 tex0   : TEXCOORD0;
};


float cmax(float3 v)
{
	return max(v.x, max(v.y, v.z));
}

// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------

OutputVS Sketch3DVS(InputVS v)
{
	// Zero output.
	OutputVS outVS = (OutputVS)0;

	float3 posW = mul(float4(v.pos.xy, 0.0f, 1.0f), gW).xyz;
	float3 prvW = mul(float4(v.dir.zw, 0.0f, 1.0f), gW).xyz;
	float3 nxtW = mul(float4(v.dir.xy, 0.0f, 1.0f), gW).xyz;

	outVS.len = v.pos.z;

	float3 posN = normalize(posW);
	float3 prvN = normalize(prvW);
	float3 nxtN = normalize(nxtW);
	float3 nxtS = normalize(cross(nxtN - posN, posN));
	float3 prvS = normalize(cross(posN - prvN, posN));
	float3 latN = normalize(nxtS + prvS) * (0.45*gWidth.x) * rsqrt(max(0.1, 0.5f + dot(nxtS, prvS)*0.5f));

	if (v.fnc[LSW]>0.5f) outVS.len = min(1, length(posN-prvN)) / gFov;
	else				 outVS.len = v.pos.z;

	float fSide = round(v.fnc[SSW] * 2.0 - 1.0);
	float fPosD = dot(posN, posW);

	posW += latN * (fSide * fPosD * gFov);
	
	outVS.color.rgba = v.clr.bgra;
	outVS.posW = float4(posW, fPosD);
	outVS.sw = v.fnc;
	outVS.posH = mul(float4(posW.xyz, 1.0f), gVP);
	outVS.tex = float4(v.dir.xy * gSize.xy, v.dir.xy * gSize.zw);

	return outVS;
}



// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------

OutputVS OrthoVS(InputVS v)
{
	// Zero output.
	OutputVS outVS = (OutputVS)0;

	float4 posH = mul(float4(v.pos.xy, 0.0f, 1.0f), gWVP);
	float4 prvH = mul(float4(v.dir.zw, 0.0f, 1.0f), gWVP);

	if (v.fnc[LSW]>0.5f) outVS.len = length((posH.xy - prvH.xy) * gTarget.zw * 0.5);
	else				 outVS.len = v.pos.z;

	outVS.posW = 0;

	if (gWide) {

		float4 nxtH = mul(float4(v.dir.xy, 0.0f, 1.0f), gWVP);
		float fSide = round(v.fnc[SSW] * 2.0 - 1.0);
		float2 pixH = gTarget.xy * gWidth.z * abs(fSide);

		nxtH.xy -= pixH;
		posH.xy -= pixH;
		prvH.xy -= pixH;

		float2 nxtS = normalize(nxtH.xy - posH.xy);
		float2 prvS = normalize(posH.xy - prvH.xy);
		float2 latW = normalize(nxtS + prvS) * (0.45*gWidth.x) * rsqrt(max(0.1, 0.5f + dot(nxtS, prvS)*0.5f));

		posH += float4(latW.y, -latW.x, 0, 0) * gTarget * fSide;
	}
	
	outVS.color.rgba = v.clr.bgra;
	outVS.sw = v.fnc;
	outVS.posH = float4(posH.xyz, 1.0f);

	outVS.tex = float4(v.dir.xy * gSize.xy, v.dir.xy * gSize.zw);

	return outVS;
}





// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------

float4 SketchpadPS(float4 sc : VPOS, OutputVS frg) : COLOR
{
	float4 t = 1;
	float3 u = 1;

	if (gFntEn)	u = tex2D(FntS, frg.tex.zw).rgb;
	if (gTexEn) t = tex2D(TexS, frg.tex.xy);	

	float f = max(u.r*0.7f, u.g);

	// Select Color source
	float4 c = frg.color;
	if (frg.sw[TSW] > 0.2f) c = gPen;
	if (frg.sw[TSW] > 0.8f) c = t;
	if (frg.sw[CSW] > 0.8f) c.a *= f;

	// Color keying
	if (gTexEn && gKeyEn) {
		float4 x = abs(c - gKey);
		if ((x.r < tol) && (x.g < tol) && (x.b < tol)) {
			if (frg.sw[CSW] > 0.2f && frg.sw[CSW] < 0.8) clip(-1);
		}
	}
	
	if (gDashEn) {
		float q;
		if (modf(frg.len*gWidth.y, q) > 0.5f) clip(-1);
	}

	if (gClipEn) {
		float3 posN = normalize(frg.posW.xyz);
		if ((dot(gPos,  posN) > gCov.x) && (frg.posW.w > gCov.y)) clip(-1);
		if ((dot(gPos2, posN) > gCov.z) && (frg.posW.w > gCov.w)) clip(-1);
	}

	if (gEffectsEn) {

		// Apply color matrix
		c = mul(c, gColorMatrix);
		
		// Apply gamma correction
		c.rgb = pow(max(c.rgb, 0), gGamma.rgb);

		// Color overboost correction beyond 0-1 range
		c.rgb += saturate(cmax(c.rgb) - 1.0f);

		// Apply noise
		float noise = (tex2D(NoiseS, sc.xy*(1.0f / 128.0f) + float2(gRandom, gRandom*7.0)).r * 2.0f) - 1.0f;
		c.rgb += lerp(float3(1, 1, 1), c.rgb, gNoiseColor.a) * gNoiseColor.rgb * noise;	
	}

	return saturate(c);
}


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
	SkpMshVS outVS = (SkpMshVS)0;
	float3 posW = mul(float4(v.posL, 1.0f), gW).xyz;
	float3 nrmW = mul(float4(v.nrmL, 0.0f), gW).xyz;
	outVS.posH  = mul(float4(posW, 1.0f), gVP);
	outVS.tex = v.tex0;
	outVS.nrmW = nrmW;
	return outVS;

}


float4 SketchMeshPS(SkpMshVS frg) : COLOR
{
	float4 cTex = 1;
	float fS = 1;
	if (gTexEn) cTex = tex2D(TexS, frg.tex);
	if (gShade) fS = dot(normalize(frg.nrmW), float3(0, 0, -1));

	cTex.rgba *= gPen.bgra;
	cTex.rgba *= gMtrl.rgba;
	cTex.rgb  *= saturate(fS);

	return cTex;
}


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
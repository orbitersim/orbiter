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
// D3D9Client ColorKey Blitting technique
// ----------------------------------------------------------------------------

uniform extern float4x4  gVP;			    // Combined World, View and Projection matrix
uniform extern texture   gTex0;			    // Diffuse texture
uniform extern float4    gColor;
uniform extern float4    gData;
uniform extern float4    gSize;
uniform extern bool      gKey;
uniform extern bool      gDash;

sampler Tex0S = sampler_state
{
	Texture   = <gTex0>;
	MinFilter = POINT;
	MagFilter = POINT;
	MipFilter = None;
	AddressU  = CLAMP;
	AddressV  = CLAMP;
};

sampler Tex0AS = sampler_state
{
	Texture   = <gTex0>;
	MinFilter = Anisotropic;
	MagFilter = Anisotropic;
	MipFilter = NONE;
	MaxAnisotropy = 18;
	AddressU  = CLAMP;
	AddressV  = CLAMP;
};

sampler Font0S = sampler_state
{
	Texture = <gTex0>;
	MinFilter = Anisotropic;
	MagFilter = LINEAR;
	MipFilter = None;
	MaxAnisotropy = 4;
	AddressU = CLAMP;
	AddressV = CLAMP;
};

struct OutputVS
{
	float4 posH    : POSITION0;
	float2 tex0    : TEXCOORD0;
};

struct DrawVS
{
	float4 posH    : POSITION0;
};


// ----------------------------------------------------------------------------
// D3D9ClientSurface Implementation
// ----------------------------------------------------------------------------

OutputVS BlitTechVS(float3 posL : POSITION0, float2 tex0: TEXCOORD0)
{
	// Zero output.
	OutputVS outVS = (OutputVS)0;
	outVS.posH = mul(float4(posL.xy-0.5f, 0.0f, 1.0f),gVP);
	outVS.tex0 = tex0;
	return outVS;
}

OutputVS FlushTechVS(float4 posL : POSITION0)
{
	// Zero output.
	OutputVS outVS = (OutputVS)0;
	outVS.posH = mul(float4(posL.xy-0.5f, 0.0f, 1.0f),gVP);
	outVS.tex0 = posL.zw*gSize.xy;
	return outVS;
}

float4 BlitTechPS(float2 tex0 : TEXCOORD0) : COLOR
{
	float4 c = tex2D(Tex0S, tex0);
	float4 x = abs(c - gColor);
	float  tol = 0.01f;
	if (x.r<tol && x.g<tol && x.b<tol) clip(-1);
	return float4(c.rgb, 1.0f);
}

float4 SketchTechPS(float2 tex0 : TEXCOORD0) : COLOR
{
	float4 c = tex2D(Tex0S, tex0);
	if (gColor.a<0) c.a = 1.0;
	else c.a *= gColor.a;
	return float4(c.rgb*gColor.rgb, c.a);
}

float4 RotateTechPS(float2 tex0 : TEXCOORD0) : COLOR
{
	float4 c = tex2D(Tex0AS, tex0);
	if (gColor.a<0) c.a = 1.0;
	else c.a *= gColor.a;
	return float4(c.rgb*gColor.rgb, c.a);
}

float4 FlushTechPS(float2 tex0 : TEXCOORD0) : COLOR
{
	float4 c = tex2D(Tex0S, tex0);

	if (gKey) {
		float4 x = abs(c - gColor);
		float  tol = 0.01f;
		if (x.r<tol && x.g<tol && x.b<tol) clip(-1);
	}

	return c;
}


technique BlitTech
{
	pass P0
	{
		vertexShader = compile vs_2_0 BlitTechVS();
		pixelShader  = compile ps_2_0 BlitTechPS();
		AlphaBlendEnable = false;
	}
}

technique SketchTech
{
	pass P0
	{
		vertexShader = compile vs_2_0 BlitTechVS();
		pixelShader  = compile ps_2_0 SketchTechPS();
		AlphaBlendEnable = true;
		BlendOp = Add;
		SrcBlend = SrcAlpha;
		DestBlend = InvSrcAlpha;
	}
}

technique RotateTech
{
	pass P0
	{
		vertexShader = compile vs_2_0 BlitTechVS();
		pixelShader  = compile ps_2_0 RotateTechPS();
		AlphaBlendEnable = true;
		BlendOp = Add;
		SrcBlend = SrcAlpha;
		DestBlend = InvSrcAlpha;
	}
}

technique FlushTech
{
	pass P0
	{
		vertexShader = compile vs_2_0 FlushTechVS();
		pixelShader  = compile ps_2_0 FlushTechPS();
		AlphaBlendEnable = false;
	}
}




// ----------------------------------------------------------------------------
// D3D9Text Implementation
// ----------------------------------------------------------------------------

OutputVS FontTechVS(float3 posL : POSITION0, float2 tex0: TEXCOORD0)
{
	// Zero output.
	OutputVS outVS = (OutputVS)0;
	outVS.posH = mul(float4(posL.xy, 0.5f, 1.0f),gVP);
	outVS.tex0 = tex0;
	return outVS;
}

float4 FontTechPS(float2 tex0 : TEXCOORD0) : COLOR
{
	float4 cTex = tex2D(Font0S, tex0);
	float a = (cTex.r+cTex.g+cTex.b)*0.33f;
	return float4(gColor.rgb, gColor.a*pow(abs(a),0.85f));
}

technique FontTech
{
	pass P0
	{
		vertexShader = compile vs_2_0 FontTechVS();
		pixelShader  = compile ps_2_0 FontTechPS();
		AlphaBlendEnable = true;
		BlendOp = Add;
		SrcBlend = SrcAlpha;
		DestBlend = InvSrcAlpha;
		ZEnable = false;
	}
}


float4 ClearTechPS(float2 tex0 : TEXCOORD0) : COLOR
{
	float4 cTex = tex2D(Font0S, tex0);
	float a = (cTex.r+cTex.g+cTex.b)*0.33f;
	return float4(gColor.rgb*cTex.rgb, gColor.a*a);
}

technique ClearTypeTech
{
	pass P0
	{
		vertexShader = compile vs_2_0 FontTechVS();
		pixelShader  = compile ps_2_0 ClearTechPS();
		AlphaBlendEnable = true;
		BlendOp = Add;
		SrcBlend = One;
		DestBlend = InvSrcAlpha;
		ZEnable = false;
	}
}





// ----------------------------------------------------------------------------
// D3D9 Sketchpad Implementation
// ----------------------------------------------------------------------------

OutputVS EllipseTechVS(float3 posL : POSITION0)
{
	// Zero output.
	OutputVS outVS = (OutputVS)0;
	float3 posX = float3(gData[0]+posL.x*gData[2], gData[1]+posL.y*gData[3], 0.0f);
	outVS.posH = mul(float4(posX, 1.0f),gVP);
	outVS.tex0 = float2(posL.z*0.13, 0);
	return outVS;
}

OutputVS LineTechVS(float3 posL : POSITION0)
{
	// Zero output.
	OutputVS outVS = (OutputVS)0;
	outVS.posH = mul(float4(posL.xy, 0.0f, 1.0f),gVP);
	outVS.tex0 = float2(posL.z*0.13, 0.0f);
	return outVS;
}

DrawVS FillTechVS(float3 posL : POSITION0)
{
	// Zero output.
	DrawVS outVS = (DrawVS)0;
	outVS.posH = mul(float4(posL.xy, 0.5f, 1.0f),gVP);
	return outVS;
}


float4 DrawTechPS(float2 tex0 : TEXCOORD0) : COLOR
{
	if (gDash) {
		float q;
		if (modf(tex0.x, q)>0.5) return gColor;
		clip(-1);
	}
	return gColor;
}

float4 FillTechPS() : COLOR
{
	return gColor;
}


technique EllipseTech
{
	pass P0
	{
		vertexShader = compile vs_2_0 EllipseTechVS();
		pixelShader  = compile ps_2_0 DrawTechPS();
		AlphaBlendEnable = true;
		BlendOp = Add;
		SrcBlend = SrcAlpha;
		DestBlend = InvSrcAlpha;
		ZEnable = false;
	}
}

technique LineTech
{
	pass P0
	{
		vertexShader = compile vs_2_0 LineTechVS();
		pixelShader  = compile ps_2_0 DrawTechPS();
		AlphaBlendEnable = true;
		BlendOp = Add;
		SrcBlend = SrcAlpha;
		DestBlend = InvSrcAlpha;
		ZEnable = false;
	}
}

technique FillTech
{
	pass P0
	{
		vertexShader = compile vs_2_0 FillTechVS();
		pixelShader  = compile ps_2_0 FillTechPS();
		AlphaBlendEnable = true;
		BlendOp = Add;
		SrcBlend = SrcAlpha;
		DestBlend = InvSrcAlpha;
		ZEnable = false;
	}
}
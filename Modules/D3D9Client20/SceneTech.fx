// =================================================================================================================================
// The MIT Lisence:
//
// Copyright (C) 2013 Jarmo Nikkanen
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

// -------------------------------------------------------------------------------------------------------------
// D3D9Client generic scene rendering technique
// -------------------------------------------------------------------------------------------------------------

uniform extern float4x4  gWVP;			    // Combined World, View and Projection matrix
uniform extern float4x4  gVP;			    // Combined World, View and Projection matrix
uniform extern float4    gColor;		    // Line Color
uniform extern texture   gTex0;			    // Diffuse texture

sampler Tex0S = sampler_state
{
	Texture = <gTex0>;
	MinFilter = Anisotropic;
	MagFilter = LINEAR;
	MipFilter = LINEAR;
	MaxAnisotropy = 8;
	AddressU = WRAP;
    AddressV = WRAP;
};


struct LineOutputVS
{
    float4 posH    : POSITION0;
};

// -------------------------------------------------------------------------------------------------------------
// Line Tech Vertex/Pixel shader implementation
// -------------------------------------------------------------------------------------------------------------

LineOutputVS LineTechVS(float3 posL : POSITION0)
{
    // Zero output.
	LineOutputVS outVS = (LineOutputVS)0;
	outVS.posH = mul(float4(posL, 1.0f),gWVP);
    return outVS;
}

float4 LineTechPS() : COLOR
{
    return gColor;
}

technique LineTech
{
    pass P0
    {
        vertexShader = compile vs_2_0 LineTechVS();
        pixelShader  = compile ps_2_0 LineTechPS();
        ZEnable = false;
        AlphaBlendEnable = false;
    }
}



// -------------------------------------------------------------------------------------------------------------
// Star rendering technique
// -------------------------------------------------------------------------------------------------------------

struct StarOutputVS
{
    float4 posH    : POSITION0;
    float4 col     : COLOR0;
};

StarOutputVS StarTechVS(float3 posL : POSITION0, float4 col : COLOR0)
{
    // Zero output.
	StarOutputVS outVS = (StarOutputVS)0;
	outVS.posH = mul(float4(posL, 1.0f), gWVP);
    outVS.col  = col;
    return outVS;
}

float4 StarTechPS(float4 col : COLOR0) : COLOR
{
    return col;
}

technique StarTech
{
    pass P0
    {
        vertexShader = compile vs_2_0 StarTechVS();
        pixelShader  = compile ps_2_0 StarTechPS();
        ZEnable = false;
        AlphaBlendEnable = false;
        ZWriteEnable = false;
    }
}
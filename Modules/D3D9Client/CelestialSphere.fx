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


struct CSphereVS
{
    float4 posH    : POSITION0;
    float4 posW    : TEXCOORD0;
    float4 angl    : TEXCOORD1;
    float4 alts    : TEXCOORD2;
    float4 c1      : TEXCOORD3;
    float2 tex0    : TEXCOORD4; 
};



CSphereVS CSphereTechVS(TILEVERTEX vrt)
{
    // Zero output.
	CSphereVS outVS = (CSphereVS)0;

    float3 v3Pos = mul(float4(vrt.posL, 1.0f), gW).xyz;
	outVS.posW = float4(v3Pos, 0.0);
	outVS.posH = mul(float4(v3Pos, 1.0f), gVP);
    outVS.tex0 = vrt.tex0;

	return outVS;
}


float4 CSphereTechPS(CSphereVS frg) : COLOR
{
    return float4(tex2D(Planet0S, frg.tex0).rgb - 0.05f, gColor.a);
}



technique SkyDomeTech
{
    pass P0
    {
        vertexShader = compile VS_MOD CSphereTechVS();
        pixelShader  = compile PS_MOD CSphereTechPS();

        AlphaBlendEnable = true;
        DestBlend = One;
        SrcBlend = SrcAlpha;
        ZEnable = false;
        ZWriteEnable = false;
    }
}
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

struct EPVERTEX {                          
    float3 posL     : POSITION0;
    float2 tex0     : TEXCOORD0;
};

struct ParticleVS
{
    float4 posH     : POSITION0;
    float2 tex0     : TEXCOORD0;
    float  light    : TEXCOORD1;
};

ParticleVS ParticleDiffuseVS(NTVERTEX vrt)
{
	ParticleVS outVS = (ParticleVS)0;
	outVS.tex0    = vrt.tex0;
    outVS.light   = saturate(dot(-gSun.direction, vrt.nrmL)) + 0.6f;
	outVS.posH    = mul(float4(vrt.posL, 1.0f), gVP);
    return outVS;
}

ParticleVS ParticleEmissiveVS(EPVERTEX vrt)
{
	ParticleVS outVS = (ParticleVS)0;
	outVS.tex0   = vrt.tex0;
	outVS.posH   = mul(float4(vrt.posL, 1.0f), gVP);
    return outVS;
}

// gMix is the particle opacity computed from time and halflife
//
float4 ParticleDiffusePS(ParticleVS frg) : COLOR
{
    float4 color = tex2D(WrapS, frg.tex0);
    return float4(color.rgb*frg.light, color.a*gMix);  
}


float4 ParticleEmissivePS(ParticleVS frg) : COLOR
{
    float4 color = tex2D(WrapS, frg.tex0);
    return float4(color.rgb*gColor.rgb, color.a*gMix);
}

float4 ParticleShadowPS(ParticleVS frg) : COLOR
{
    float4 color = tex2D(WrapS, frg.tex0);
    return float4(0,0,0,color.a*gMix);
}

technique ParticleDiffuseTech
{
    pass P0
    {
        vertexShader = compile VS_MOD ParticleDiffuseVS();
        pixelShader  = compile PS_MOD ParticleDiffusePS();

        AlphaBlendEnable = true;
        BlendOp = Add;
        ZEnable = true; 
        SrcBlend = SrcAlpha;
        DestBlend = InvSrcAlpha;    
        ZWriteEnable = false;
    }
}


technique ParticleEmissiveTech
{
    pass P0
    {
        vertexShader = compile VS_MOD ParticleEmissiveVS();
        pixelShader  = compile PS_MOD ParticleEmissivePS();

        AlphaBlendEnable = true;
        BlendOp = Add;
        ZEnable = true; 
        SrcBlend = SrcAlpha;
        DestBlend = InvSrcAlpha;    
        ZWriteEnable = false;
    }

    pass P1
    {
        vertexShader = compile VS_MOD ParticleEmissiveVS();
        pixelShader  = compile PS_MOD ParticleShadowPS();

        AlphaBlendEnable = true;
        BlendOp = Add;
        ZEnable = true; 
        SrcBlend = SrcAlpha;
        DestBlend = InvSrcAlpha;    
        ZWriteEnable = false;
    }
}
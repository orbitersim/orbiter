
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
// ===================================================
// Copyright (C) 2022 Jarmo Nikkanen
// licensed under MIT
// ===================================================


float ilerp(float a, float b, float x)
{
	return saturate((x - a) / (b - a));
}


struct MESH_VERTEX {
	float3 posL   : POSITION0;
	float3 nrmL   : NORMAL0;
	float3 tanL   : TANGENT0;
	float3 tex0   : TEXCOORD0;
};

sampler tTex;

uniform extern struct {
	float4x4 mVP;
	float4x4 mW;
} cbPS;


struct MData {
	float4 posH : POSITION0;
	float3 camW : TEXCOORD0;
};

MData StageVS(MESH_VERTEX vrt)
{
	MData outVS = (MData)0;
	float3 posW = mul(float4(vrt.posL, 1.0f), cbPS.mW).xyz;
	outVS.posH = mul(float4(posW, 1.0f), cbPS.mVP);
	outVS.camW = -posW;
	return outVS;
}

float4 StagePS(MData frg) : COLOR
{
	float3 cA = texCUBElod(tTex, float4(normalize(-frg.camW), 0.0)).rgb;
	return float4(cA, 1);
}
	


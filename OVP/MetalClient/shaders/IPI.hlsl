
uniform extern float4x4  mVP;
uniform extern float4    vTgtSize;
uniform extern float4    vPos;

struct OutputVS
{
	float4 posH     : POSITION0;
	float  x    	: TEXCOORD0;
	float  y    	: TEXCOORD1;
};


OutputVS VSMain(float3 posL : POSITION0, float2 tex0: TEXCOORD0)
{
	// Zero output.
	OutputVS outVS = (OutputVS)0;

	posL.xy *= vPos.xy;
	posL.xy += vPos.zw;
	posL.xy *= vTgtSize.xy;
	
	outVS.posH = mul(float4(posL.xy-0.5f, 0.0f, 1.0f), mVP);
	outVS.x = tex0.x;
	outVS.y = tex0.y;
	return outVS;
}




// Example of pixel shader

sampler mySmp;

float4 PSMain(float x : TEXCOORD0, float y : TEXCOORD1) : COLOR
{
	return tex2D(mySmp, float2(x,y));
}

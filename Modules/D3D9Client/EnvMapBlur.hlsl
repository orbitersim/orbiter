
uniform extern float2  vaKernel[32];
uniform extern float2  vaRotationX[32];
uniform extern float2  vaRotationY[32];
uniform extern float3  vDir;
uniform extern float3  vUp;
uniform extern float3  vCp;
uniform extern float   fScale;
uniform extern float   fD;
uniform extern int     iCount;

sampler tCube;
sampler tSrc;

float4 PSDither(float x : TEXCOORD0, float y : TEXCOORD1, int2 tgt : VPOS) : COLOR
{
	float3 color = 0;
	
	x = x * 2.0f - 1.0f;									// Conver range to [-1 to 1]
	y = y * 2.0f - 1.0f;
	
	float2 rt  = vaRotationX[tgt.x%32];
	float3 dir = vDir - vUp*y + vCp*x;						// Cube map sample direction
	float3 upq = (vUp * rt.x + vCp * rt.y);					// Kernel rotation axes
	float3 cpq = (vCp * rt.x - vUp * rt.y);					// Kernel rotation axes
	
	rt  = vaRotationY[tgt.y%32];
	float3 up  = (upq * rt.x + cpq * rt.y) * fScale;					// Kernel rotation axes
	float3 cp  = (cpq * rt.x - upq * rt.y) * fScale;
	
	for (int i=0;i<iCount;i++) {
		float3 p = dir + (up * vaKernel[i].y + cp * vaKernel[i].x);
		color += texCUBE(tCube, p).rgb;
	}
	
	color/=iCount;								
	return float4(color, 1.0f);
}


// ======================================================================================================

float4 PSBlur(float x : TEXCOORD0, float y : TEXCOORD1) : COLOR
{
	float3 dir; 
	float3 color = 0;
	
	x = x * 2.0f - 1.0f;
	y = y * 2.0f - 1.0f;
	
	//---------------------------------------------------
	dir = vDir - vUp*(y+fD);
	color += texCUBE(tSrc, dir + vCp*(x+fD)).rgb;
	color += texCUBE(tSrc, dir + vCp*(x)).rgb;
	//---------------------------------------------------
	dir = vDir - vUp*(y);
	color += texCUBE(tSrc, dir + vCp*(x+fD)).rgb;
	color += texCUBE(tSrc, dir + vCp*(x)).rgb;
	//---------------------------------------------------
	return float4(color*0.25f, 1.0f);
}

/*float4 PSBlur(float x : TEXCOORD0, float y : TEXCOORD1) : COLOR
{
	float3 dir; 
	float3 color = 0;
	
	x = x * 2.0f - 1.0f;
	y = y * 2.0f - 1.0f;
	
	//---------------------------------------------------
	dir = vDir - vUp*(y+fD);
	color += texCUBE(tSrc, dir + vCp*(x+fD)).rgb;
	color += texCUBE(tSrc, dir + vCp*(x)).rgb;
	color += texCUBE(tSrc, dir + vCp*(x-fD)).rgb;
	//---------------------------------------------------
	dir = vDir - vUp*(y);
	color += texCUBE(tSrc, dir + vCp*(x+fD)).rgb;
	color += texCUBE(tSrc, dir + vCp*(x)).rgb;
	color += texCUBE(tSrc, dir + vCp*(x-fD)).rgb;
	//---------------------------------------------------
	dir = vDir - vUp*(y-fD);
	color += texCUBE(tSrc, dir + vCp*(x+fD)).rgb;
	color += texCUBE(tSrc, dir + vCp*(x)).rgb;
	color += texCUBE(tSrc, dir + vCp*(x-fD)).rgb;
	//---------------------------------------------------
	return float4(color*0.11111, 1.0f);
}*/


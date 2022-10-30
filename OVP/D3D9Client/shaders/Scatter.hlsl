
// ============================================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// licensed under LGPL v2
// Copyright (C) 2021 Jarmo Nikkanen
// ============================================================================

#define Nc  16		//Z-dimension count in 3D texture
#define Wc  88		//3D texture size (pixels)
#define Qc  180		//2D texture size (pixels)


#define NSEG 12
#define iNSEG 1.0f / NSEG;
#define MINANGLE -0.3f			// Minumum angle
#define ANGRNG (1.0f - MINANGLE)
#define iANGRNG (1.0f / ANGRNG)
#define xANGRNG (2.0f * ANGRNG)
#define ixANGRNG (1.0f / xANGRNG)
#define MAXA (Prm.RaH * 2.0f)	// Maximum altitude
#define PI 3.14159265
#define BOOL bool



#define GLARE_SIZE 4.0f
#define MieMin  0.01f

// Per-Frame Params
//
struct AtmoParams
{
	float4x4 mVP;				// View Projection Matrix
	float3 CamPos;				// Geocentric Camera position
	float3 toCam;				// Geocentric Camera direction (unit vector)
	float3 toSun;				// Geocentric Sun direction (unit vector)
	float3 SunAz;				// Atmo scatter ref.frame (unit vector) (toCam, ZeroAz, SunAz)
	float3 ZeroAz;				// Atmo scatter ref.frame (unit vector)
	float3 Up;					// Sun Ref Frame (Unit Vector) (Up, toSun, ZeroAz)
	float3 vTangent;			// Reference frame for narmal mapping (Unit Vector)
	float3 vBiTangent;			// Reference frame for narmal mapping (Unit Vector)
	float3 vPolarAxis;			// North Pole (unit vector)
	float3 cSun;				// Sun Color and intensity
	float3 RayWave;				// .rgb Rayleigh Wave lenghts
	float3 MieWave;				// .rgb Mie Wave lenghts
	float3 GlareColor;
	float4 HG;					// Henyey-Greenstein Phase function params
	float2 iH;					// Inverse scale height for ray(.r) and mie(.g) exp(-altitude * iH) 
	float2 rmO;					// Ray and Mie out-scatter factors
	float2 rmI;					// Ray and Mie in-scatter factors
	float  RayPh;				// Phase
	float  PlanetRad;			// Planet Radius
	float  PlanetRad2;			// Planet Radius Squared
	float  AtmoAlt;				// Atmospehere upper altitude limit
	float  AtmoRad;				// Atmospehere outer radius
	float  AtmoRad2;			// Atmospehere outer radius squared
	float  CloudAlt;			// Cloud layer altitude 
	float  MinAlt;				// Minimum terrain altitude
	float  MaxAlt;				// Maximum terrain altitude
	float  iAltRng;				// 1.0 / (MaxAlt - MinAlt);
	float  AngMin;
	float  AngRng;
	float  iAngRng;
	float  AngCtr;				// Cos of View cone angle from planet center that's visible from camera location 
	float  HrzDst;				// Distance to horizon (500 m) minimum if camera below sea level
	float  CamAlt;				// Camera Altitude
	float  CamRad;				// Camera geo-distance
	float  CamRad2;				// Camera geo-distance squared
	float  MaxDst;				// Max "ray" distance through atmosphere
	float  iMaxDst;
	float  Expo;				// "HDR" exposure factor (atmosphere only)
	float  Time;				// Simulation time / 180
	float  TrGamma;				// Terrain "Gamma" correction setting
	float  TrExpo;				// "HDR" exposure factor (terrain only)
	float  Ambient;				// Global ambient light level
	float  Clouds;
	float  Glare;
	float  TW_Multi;
	float  TW_Haze;
	float  TW_Dst;
	float  SunRadAtHrz;
	float  CamSpace;			// Camera in space scale factor 0.0 = surf, 1.0 = space
};

struct sFlow {
	BOOL bRay;
	BOOL bAmb;
};

uniform extern AtmoParams Const;
uniform extern sFlow Flo;
uniform extern bool bInSpace;

sampler2D tSun;
sampler2D tCam;
sampler2D tLndRay;
sampler2D tLndMie;
sampler2D tLndAmb;
sampler2D tSunGlare;
sampler2D tAmbient;
sampler2D tSkyRayColor;
sampler2D tSkyMieColor;

//static const float n[] = { 0.0254465, 0.129234, 0.297077, 0.5, 0.702923, 0.870766, 0.974554 };
//static const float w[] = { 0.129485, 0.279705, 0.381830, 0.417959,  0.381830, 0.279705, 0.129485 };

static const float n[] = { 0.0714, 0.21428, 0.35714, 0.5, 0.64285, 0.78571, 0.92857 };
static const float w[] = { 0.14286, 0.14286, 0.14286, 0.14286,  0.14286, 0.14286, 0.14286 };

static const float nb[] = { 0.1127, 0.5, 0.8873 };
static const float wb[] = { 0.5555555, 0.888888, 0.555555 };


float2 NrmToUV(float3 vNrm)
{
	float2 uv = float2(dot(Const.ZeroAz, vNrm), dot(Const.SunAz, vNrm)) / Const.AngCtr;
	return uv * 0.5 + 0.5;
}


float3 uvToLoc(float2 uv, float r)
{
	uv = (uv * 2.0 - 1.0) * r;
	float det = 1.0f - uv.x * uv.x - uv.y * uv.y;
	float cos_ab = det > 0.0f ? sqrt(det) : 0.0f;
	return float3(uv.x, uv.y, cos_ab);
}


float3 uvToNrm(float2 uv)
{
	float3 q = uvToLoc(uv, Const.AngCtr);
	return normalize(Const.SunAz * q.y + Const.ZeroAz * q.x + Const.toCam * q.z);
}


float3 uvToRay(float2 uv)
{
	float3 q = uvToLoc(uv, 1.0f);
	return normalize(Const.SunAz * q.y + Const.ZeroAz * q.x + Const.toCam * q.z);
}


float3 uvToDir(float2 uv)
{
	uv.y = uv.y * rsqrt(0.2f + uv.y * uv.y) * sqrt(1.2f);

	float x = uv.x * 2.0f - 1.0f;
	float y = sqrt(1.0f - x * x);
	float z = 1.0f - uv.y * Const.AngRng;
	float k = sqrt(1.0f - z * z);

	return normalize(Const.SunAz * x * k + Const.ZeroAz * y * k + Const.toCam * z);
}


float2 DirToUV(float3 uDir)
{
	float y = dot(uDir, Const.toCam);
	float3 uOrt = normalize(uDir - Const.toCam * y);
	float x = dot(uOrt, Const.SunAz) * 0.5 + 0.5;
	float2 uv = float2(x, 1.0f - (y - Const.AngMin) * Const.iAngRng);

	uv.y = sqrt(0.2f) * uv.y * rsqrt(1.2f - uv.y * uv.y);
	return uv;
}


float PlanetShadowFactor(float3 vPos, float segh)
{
	float d = dot(vPos, Const.toSun);
	float h = sqrt(dot(vPos, vPos) - d * d) - Const.PlanetRad;
	return d > 0 ? 1.0f : saturate(1.0f + h / segh);
}


float3 HDR(float3 x)
{
	return 1.0f - exp(-Const.Expo * x);
}


float3 LightFX(float3 x)
{
	//return x * rsqrt(1.0f + x*x) * 1.4f;
	return 2.0 * x / (1.0f + x);
}


float RayLength(float cos_dir, float r0, float r1)
{
	float y = r0 * cos_dir;
	float z2 = r0 * r0 - y * y;
	return sqrt(r1 * r1 - z2) - y;
}


float RayLength(float3 vPos)
{
	float y = dot(vPos, Const.toCam);
	float z2 = dot(vPos, vPos) - y * y;
	return sqrt(Const.AtmoRad2 - z2) - y;
}


float RayLength(float cos_dir, float r0)
{
	return RayLength(cos_dir, r0, Const.AtmoRad);
}


// Compute UV and blend factor for smaple3D routine
//
float3 TransformUV(float3 uv, const float rc, const float prc)
{
	const float ipix = 1.0f / (rc * prc - 1.0f);

	float z = uv.z * (rc - 1) * 0.999999f;
	uv = saturate(uv);
	uv.x *= prc * ipix - ipix;
	uv.x += prc * floor(z) * ipix;

	return uv;
}


float4 smaple3D(sampler2D tSamp, float3 uv, const float rc, const float pix)
{
	const float x = pix / (rc * pix - rc);

	float4 a = tex2D(tSamp, uv.xy).rgba;
	float4 b = tex2D(tSamp, uv.xy + float2(x, 0)).rgba;
	return lerp(a, b, frac(uv.z));
}



// Optical depth integral from point in atmosphere to infinity.
//
float2 Gauss7(float cos_dir, float r0, float2 ih0)
{
	int i;
	float y = r0 * cos_dir;
	float z2 = r0 * r0 - y * y;
	float Ray = sqrt(Const.AtmoRad2 - z2) - y; // Length of the ray
	
	// Compute altitudes of sample points
	float a[7];
	for (i = 0; i < 7; i++) {
		float p = Ray * n[i] + y;
		a[i] = sqrt(z2 + p * p) - Const.PlanetRad;
	}

	float2 sum = 0.0f;
	for (i = 0; i < 7; i++) sum += exp(-clamp(a[i] * ih0, -20, 20)) * w[i];
	return sum * Ray * 0.5f;
}


// Optical depth integral in atmosphere for a given distance
//
float2 Gauss7(float cos_dir, float r0, float dist, float2 ih0)
{
	int i;
	// Compute altitudes of sample points
	float a[7];
	for (i = 0; i < 7; i++) {
		float d = dist * n[i];
		a[i] = sqrt(r0 * r0 + d * d - 2.0 * r0 * d * cos_dir) - Const.PlanetRad;
	}

	float2 sum = 0.0f;
	for (i = 0; i < 7; i++) sum += exp(-clamp(a[i] * ih0, -20, 20)) * w[i];
	return sum * dist * 0.5f;
}


// Rayleigh phase function
//
float RayPhase(float cw)
{
	return 0.25f * (4.0f + cw * cw) / (1.0f + Const.RayPh * cw);
}


// Henyey-Greenstein Phase function
//
float MiePhase(float cw)
{
	float cw2 = cw * cw;
	return Const.HG.x * (1.0f + cw2) * pow(abs(Const.HG.y - Const.HG.z * cw2*cw), -1.5f) + Const.HG.w;
}


float MiePhase2(float cw, float g)
{
	float cw2 = cw * cw;
	return (1.0f - g*g) * (1.0f + cw2) * pow(abs(1.0f + g * g - 2.0f * g * cw), -1.5f);
}


// 2D lookup-table, for direct sunlight being filtered by atmosphere. (constant), doesn't vary with camera movement.
//
float4 SunColor(float x : TEXCOORD0, float y : TEXCOORD1) : COLOR
{
	float alt = lerp(Const.MinAlt, Const.AtmoAlt, y*y);
	float ang = x * ANGRNG + MINANGLE;

	// Compute shadowing caused by a planet
	//float s = (alt + Const.PlanetRad) * sqrt(1.0f - ang * ang) - Const.PlanetRad; // Sun altitude above horizon (meters)

	float2 rm = Gauss7(ang, alt + Const.PlanetRad, Const.iH) * Const.rmO;
	float3 clr = Const.RayWave * rm.r + Const.MieWave * rm.g;

	return float4(exp(-clr), 1.0f);
}


float3 GetSunColor(float dir, float alt)
{
	alt = saturate((alt - Const.MinAlt) / Const.AtmoAlt);
	dir = (dir - MINANGLE) * iANGRNG;
	alt = sqrt(abs(alt)) * sign(alt);
	return tex2D(tSun, float2(dir, alt)).rgb;
}


float SunGlare(float3 vRay)
{
	if (dot(vRay, Const.toSun) < 0) return 0.0f;
	float2 uv = float2(dot(vRay, Const.ZeroAz), dot(vRay, Const.Up)) * GLARE_SIZE + 0.5f;
	float c = tex2D(tSunGlare, uv).r * Const.Glare * 0.2f;
	return c;
}


float3 ComputeCameraView(float3 vNrm, float3 vRay, float r, float d)
{
	float a = dot(vNrm, vRay);
	float2 rm = Gauss7(-a, r, d, Const.iH) * Const.rmO;
	float3 clr = Const.RayWave * rm.r + Const.MieWave * rm.g;
	return exp(-clr);
}


float3 MultiScatterApprox(float3 vNrm)
{
	float dNS = clamp(dot(vNrm, Const.toSun) + Const.TW_Dst, 0.0f, Const.TW_Dst);
	return (Const.RayWave + 0.3f) * Const.TW_Multi * dNS;
}


struct SkyOut
{
	float4 ray;
	float4 mie;
};


SkyOut GetSkyColor(float3 uDir)
{
	float2 uv = DirToUV(uDir);

	SkyOut o;
	o.ray = tex2D(tSkyRayColor, uv).rgba;
	o.mie = tex2D(tSkyMieColor, uv).rgba;

	return o;
}


// 2D lookup-table for pre-computed sky color. (pre frame), varies with camera pos and sun pos
//
float4 SkyView(float u : TEXCOORD0, float v : TEXCOORD1) : COLOR
{
	// Viewing ray
	float3 vRay = uvToDir(float2(u,v));

	// Length of the viewing ray
	float len = RayLength(dot(vRay, Const.toCam), Const.CamRad);

	float3 vNrm = normalize(Const.CamPos + vRay * len);

	float rmO = Flo.bRay ? Const.rmO.r : Const.rmO.g;
	float rmI = Flo.bRay ? Const.rmI.r : Const.rmI.g;
	float iH = Flo.bRay ? Const.iH.r : Const.iH.g;

	float3 ret = 0;

	for (int i = 0; i < 7; i++)
	{
		float  dst = len * n[i];
		float3 pos = Const.CamPos + vRay * dst;
		float3 n = normalize(pos);
		float  dRS = dot(n, Const.toSun);
		float  rad = dot(n, pos);
		float  alt = rad - Const.PlanetRad;
		float3 x = GetSunColor(dRS, alt) * ComputeCameraView(Const.toCam, vRay, Const.CamRad, dst);
		float f = exp(-alt * iH) * w[i];
		ret += x * f;
	}

	ret *= Const.cSun;
	ret *= rmI * len * 0.5f;
	ret *= Flo.bRay ? Const.RayWave : Const.MieWave;

	float alpha = saturate(1.0f - ComputeCameraView(Const.toCam, vRay, Const.CamRad, len).g);

	return float4(ret, alpha);
}


// 2D lookup-table for pre-computed sky color. (pre frame), varies with camera pos and sun pos
//
float4 RingView(float u : TEXCOORD0, float v : TEXCOORD1) : COLOR
{

	float x = -1.0f + u * 2.0f;
	float y = sqrt(1.0f - x * x);

	float r = Const.PlanetRad + v * Const.AtmoAlt;
	float d = sqrt(Const.CamRad2 + r * r - 2.0f * r * Const.PlanetRad);
	float t = Const.CamRad * (Const.CamRad2 + d * d - r * r) / (2.0f * Const.CamRad * d);
	
	r = sqrt(Const.CamRad2 - t * t);

	float l = 2.0f * sqrt(Const.AtmoRad2 - r * r); // Ray length through atmosphere
	float h = sqrt(Const.CamRad2 - r * r);	// Horizon distance at alt of "r"
	float z = -h / Const.CamRad;	// Cosine of viewing angle
	float k = sqrt(1.0f - z * z);
	float q = h - l * 0.5f;			// Distance to atmosphere threshold

	// Viewing ray
	float3 vRay = normalize(Const.SunAz * x * k + Const.ZeroAz * y * k + Const.toCam * z);

	float Ph = dot(Const.toSun, Const.toCam);

	float rmO = Flo.bRay ? Const.rmO.r : Const.rmO.g;
	float rmI = Flo.bRay ? Const.rmI.r : Const.rmI.g;
	float iH = Flo.bRay ? Const.iH.r : Const.iH.g;

	float3 ret = 0;

	for (int i = 0; i < 7; i++)
	{
		float  dst = l * n[i];
		float3 pos = Const.CamPos + vRay * (q + dst);
		float3 n = normalize(pos);
		float  dRS = dot(n, Const.toSun);
		float  rad = dot(n, pos);
		float  alt = rad - Const.PlanetRad;

		float3 x = GetSunColor(dRS, alt) * ComputeCameraView(Const.toCam, vRay, Const.CamRad, dst);

		float f = exp(-alt * iH) * w[i];
		ret += x * f;
	}

	ret *= Const.cSun;
	ret *= rmI * l * 0.5f;
	ret *= Flo.bRay ? Const.RayWave : Const.MieWave;

	float alpha = saturate(1.0f - ComputeCameraView(Const.toCam, vRay, Const.CamRad, l).g);

	return float4(ret, alpha);
}



float3 GetAmbient(float3 vRay)
{
	return tex2D(tAmbient, DirToUV(vRay)).rgb;
}


// 2D lookup-table for pre-computed ambient sky color. (pre frame), varies with sun/cam relation
//
float4 AmbientSky(float u : TEXCOORD0, float v : TEXCOORD1) : COLOR
{
	// Viewing ray
	float2 uv = float2(u, v);

	float3 uDir = uvToDir(uv);

	float3 ray = tex2D(tSkyRayColor, uv).rgb;
	float3 mie = tex2D(tSkyMieColor, uv).rgb;
	float3 cMlt = MultiScatterApprox(Const.toCam);
	float3 cGlr = max(MieMin, mie) * SunGlare(-uDir) * Const.GlareColor;

	float ph = dot(uDir, Const.toSun);

	float3 color = ray * RayPhase(ph) + mie * MiePhase(ph) + cMlt + cGlr;

	return float4(color, 1.0f);
}



struct LandOut
{
	float4 ray;
	float4 mie;
	float4 amb;
};


LandOut GetLandView(float rad, float3 vNrm)
{
	float2 uv = NrmToUV(vNrm);

	float a = rad - Const.PlanetRad;
	float z = saturate((a - Const.MinAlt) * Const.iAltRng); // inverse lerp

	float3 uvb = TransformUV(float3(uv, z), Nc, Wc);

	LandOut o;
	o.ray = smaple3D(tLndRay, uvb, Nc, Wc);
	o.mie = smaple3D(tLndMie, uvb, Nc, Wc);
	o.amb = smaple3D(tLndAmb, uvb, Nc, Wc);
	return o;
}


// Render 3D Lookup texture for land view
//
float4 LandView(float u : TEXCOORD0, float v : TEXCOORD1) : COLOR
{
	u *= 16.0f;
	float a = floor(u) / 16.0f;
	float alt = lerp(Const.MinAlt, Const.MaxAlt, a);
	float r = alt + Const.PlanetRad;

	// Geo-centric Vertex location
	float3 vNrm = uvToNrm(float2(frac(u), v));
	float3 vVrt = vNrm * r;

	// Viewing ray
	float3 vCam = Const.CamPos - vVrt;
	float3 vRay = normalize(vCam); // Towards camera from vertex

	float ang = dot(vNrm, vRay);
	float len = RayLength(ang, r);

	if (!Flo.bRay) len = min(len, 10e3);

	float dist = min(len, dot(vRay, vCam));
	float dSR = dot(Const.toSun, vRay);
	float segh = sqrt(1.0 - dSR * dSR) * dist * iNSEG;

	float3 ret = 0;
	float osc = 0;

	

	float rmO = Flo.bRay ? Const.rmO.r : Const.rmO.g;
	float rmI = Flo.bRay ? Const.rmI.r : Const.rmI.g;
	float iH  = Flo.bRay ? Const.iH.r : Const.iH.g;

	for (int i = 0; i < NSEG; i++)
	{
		//float dst = dist * n[i];
		float dst = dist * i * iNSEG;
		float3 pos = vVrt + vRay * dst;
		float3 n = normalize(pos);
		float rad = dot(n, pos);
		float alt = rad - Const.PlanetRad;
		float3 x = ComputeCameraView(n, vRay, rad, dist - dst);

		if (Flo.bAmb) x *= MultiScatterApprox(n) * Const.TW_Haze * exp(-alt * iH);
		else
		{
			x *= GetSunColor(dot(n, Const.toSun), alt);
			x *= PlanetShadowFactor(pos, segh);
		}

		//float f = exp(-alt * iH) * w[i];
		float f = exp(-alt * iH) * iNSEG;

		osc += f;
		ret += x * f;
	}

	//dist *= 0.5;
	ret *= Const.cSun;
	osc *= rmO * dist;
	ret *= rmI * dist;
	ret *= Flo.bRay ? Const.RayWave : Const.MieWave;

	if (Flo.bAmb) {
		ret = HDR(ret);
		float b = pow(saturate(dot(ret, ret)), 0.2f);
		return float4(b, b, b, 1);
	}

	return float4(ret, osc);
}





// Render Sun Texture "Glare"
//
float4 RenderSun(float u : TEXCOORD0, float v : TEXCOORD1) : COLOR
{
	u = u * 2.0 - 1.0;
	v = v * 2.0 - 1.0;

	float a = atan2(u, v);
	float r = sqrt(u * u + v * v);

	float q = 0.5f + 0.5f * pow(sin(3.0f * a), 4.0f);
	float w = 0.5f + 0.3f * pow(sin(30.0f * a), 2.0f) * pow(sin(41.0f * a), 2.0f);

	//float I = pow(max(0, 2.0f * (1 - r / q)), 12.0f);
	//float K = pow(max(0, 2.0f * (1 - r / w)), 12.0f);

	float I = pow(max(0, (1 - r / q)), 6.0f) * 8e4;
	float K = pow(max(0, (1 - r / w)), 6.0f) * 16e4;

	//float I = exp(max(0, 10.0f * (1 - r / q))) - 1.0f;
	//float K = exp(max(0, 10.0f * (1 - r / w))) - 1.0f;

	float L = exp(max(0, 0.35f - r) * 43.0f);

	return float4(max(I+L,K+L), 0, 0, 1);
}



/*

float4 LandView(float u : TEXCOORD0, float v : TEXCOORD1) : COLOR
{
	u *= 16.0f;
	float a = floor(u) / 16.0f;
	float alt = lerp(Const.MinAlt, Const.MaxAlt, a);
	float r = alt + Const.PlanetRad;

	// Geo-centric Vertex location
	float3 vNrm = uvToNrm(float2(frac(u), v));
	float3 vVrt = vNrm * r;

	// Viewing ray
	float3 vCam = Const.CamPos - vVrt;
	float3 vRay = normalize(vCam); // Towards camera from vertex

	float ang = dot(vNrm, vRay);
	float dist = min(RayLength(ang, r), dot(vRay, vCam));

	float3 ret = 0;
	float osc = 0;

	float3 cMlt = (Const.idx == 0) ? 0 : MultiScatterApprox(Const.toCam) * Const.Multi;

	for (int i = 0; i < 7; i++)
	{
		float dst = dist * n[i];
		float3 pos = vVrt + vRay * dst;
		float3 n = normalize(pos);
		float dRS = dot(n, Const.toSun);
		float rad = dot(n, pos);
		float alt = rad - Const.PlanetRad;
		float iH = Const.iH[Const.idx];
		float3 x = (GetSunColor(dRS, alt) + cMlt) * ComputeCameraView(n, vRay, rad, dist-dst);

		float f = exp(-alt * Const.iH[Const.idx]) * w[i];
		osc += f;
		ret += x * f;
	}

	dist *= 0.5;
	ret *= Const.cSun;
	osc *= Const.rmO[Const.idx] * dist;
	ret *= Const.rmI[Const.idx] * dist;
	ret *= (Const.idx == 0) ? Const.RayWave : Const.MieWave;

	return float4(ret, osc);
}
*/

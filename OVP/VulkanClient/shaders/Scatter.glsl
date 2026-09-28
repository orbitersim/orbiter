

// ============================================================================
// Part of the ORBITER VISUALISATION PROJECT (OVP)
// licensed under MIT
// Copyright (C) 2022-2026 Jarmo Nikkanen
// ============================================================================

#if defined(_PERFORMANCE) // DO NOT CHANGE THESE, MUST MATCH WITH C++ CODE
#define Nc  6		//Z-dimension count in 3D texture
#define Wc  72		//3D texture size (pixels)
#define Qc  64		//2D texture size (pixels)
#else
#define Nc  8		//Z-dimension count in 3D texture
#define Wc  128		//3D texture size (pixels)
#define Qc  96		//2D texture size (pixels)
#endif


#define NSEG 5
#define iNSEG 1.0f / NSEG
#define MINANGLE -0.33f			// Minumum angle
#define ANGRNG (0.25f - MINANGLE)
#define iANGRNG (1.0f / ANGRNG)
#define BOOL bool
#define LastLine (0.999999f - 1.0f / Qc)


// Per-Frame Params
//
struct AtmoParams
{
	mat4 mVP;				// View Projection Matrix
	vec3 CamPos;				// Geocentric Camera position
	vec3 toCam;				// Geocentric Camera direction (unit vector)
	vec3 toSun;				// Geocentric Sun direction (unit vector)
	vec3 SunAz;				// Atmo scatter ref.frame (unit vector) (toCam, ZeroAz, SunAz)
	vec3 ZeroAz;				// Atmo scatter ref.frame (unit vector)
	vec3 Up;					// Sun/Shadow Ref Frame (Unit Vector) (Up, toSun, ZeroAz)
	vec3 vTangent;			// Reference frame for normal mapping (Unit Vector)
	vec3 vBiTangent;			// Reference frame for normal mapping (Unit Vector)
	vec3 vPolarAxis;			// North Pole (unit vector)
	vec3 cSun;				// Sun Color and intensity
	vec3 RayWave;				// .rgb Rayleigh Wave lenghts
	vec3 MieWave;				// .rgb Mie Wave lenghts
	vec4 HG;					// Henyey-Greenstein Phase function params
	vec2 iH;					// Inverse scale height for ray(.r) and mie(.g) e.g. exp(-altitude * iH) 
	vec2 rmO;					// Ray and Mie out-scatter factors
	vec2 rmI;					// Ray and Mie in-scatter factors
	vec3 cAmbient;			// Ambient light color at sealevel
	vec3 cGlare;				// Sun glare color
	float  PlanetRad;			// Planet Radius
	float  PlanetRad2;			// Planet Radius Squared
	float  AtmoAlt;				// Atmospehere upper altitude limit
	float  AtmoRad;				// Atmospehere outer radius
	float  AtmoRad2;			// Atmospehere outer radius squared
	float  CloudAlt;			// Cloud layer altitude for color and light calculations (not for phisical rendering) 
	float  MinAlt;				// Minimum terrain altitude
	float  MaxAlt;				// Maximum terrain altitude
	float  iAltRng;				// 1.0 / (MaxAlt - MinAlt);
	float  AngMin;
	float  AngRng;
	float  iAngRng;
	float  AngCtr;				// Cos of View cone angle from planet center that's visible from camera location 
	float  HrzDst;				// Distance to horizon (500 m) minimum if camera below sea level
	float  CamAlt;				// Camera Altitude
	float  CamElev;				// Camera Elevation above surface
	float  CamRad;				// Camera geo-distance
	float  CamRad2;				// Camera geo-distance squared
	float  Expo;				// "HDR" exposure factor (atmosphere only)
	float  Time;				// Simulation time / 180
	float  TrGamma;				// Terrain "Gamma" correction setting
	float  TrExpo;				// "HDR" exposure factor (terrain only)
	float  Ambient;				// Global ambient light level
	float  Clouds;				// Cloud layer intensity (if below), and Blue light inscatter scale factor (if camera Above clouds)
	float  TW_Terrain;			// Twilight intensity
	float  TW_Dst;				// Twilight distance behind terminator
	float  CosAlpha;			// Cosine of camera horizon angle i.e. PlanetRad/CamRad
	float  SinAlpha;			// Sine of ^^
	float  CamSpace;			// Camera in space scale factor 0.0 = surf, 1.0 = space
	float  Cr2;					// Camera radius on shadow plane (dot(cp.toCam, cp.Up) * cp.CamRad)^2
	float  ShdDst;
	float  SunVis;
	float  dCS;
	float  smi;
	float  ecc;
	float  trLS;
	float  wNrmStr;				// Water normal strength
	float  wSpec;				// Water smoothness
	float  wBrightness;
	float  wBoost;
};

struct sFlow {
	BOOL bRay;					// True for rayleigh render pass
	BOOL bCamLit;				// True if camera is lit by sunlight
	BOOL bCamInSpace;			// True if camera is in space (i.e. not in atmosphere)
};

// uniform extern AtmoParams Const, sFlow Flo: pixel shader constants (binding 1; NewPlanet's vertex shaders read Const too)
layout(binding = 1, row_major, scalar) uniform AtmoBlock { AtmoParams Const; sFlow Flo; };

layout(binding = 4) uniform sampler2D tSun;		// s0
layout(binding = 5) uniform sampler2D tCam;
layout(binding = 6) uniform sampler2D tLndRay;
layout(binding = 7) uniform sampler2D tLndMie;
layout(binding = 8) uniform sampler2D tLndAtn;
layout(binding = 9) uniform sampler2D tSunGlare;
layout(binding = 10) uniform sampler2D tAmbient;
layout(binding = 11) uniform sampler2D tSkyRayColor;
layout(binding = 12) uniform sampler2D tSkyMieColor;

#if NSEG == 5
const float n[] = float[](0.050, 0.25, 0.50, 0.75, 0.950);
const float w[] = float[](0.125, 0.25, 0.25, 0.25, 0.125);
#endif

#if NSEG == 7
const float n[] = float[](0.05, 0.167, 0.333, 0.500, 0.667, 0.833, 0.95);
const float w[] = float[](0.08, 0.167, 0.167, 0.167, 0.167, 0.167, 0.08);
#endif

// Gauss7 points and weights
const vec4 n0 = vec4(0.0714, 0.21428, 0.35714, 0.5 );
const vec4 w0 = vec4(0.1295, 0.27971, 0.38183, 0.41796);
const vec4 n1 = vec4(0.64285, 0.78571, 0.92857, 0 );
const vec4 w1 = vec4(0.38183, 0.27971, 0.1295, 0 );

// Gauss4 points and weights
const vec4 n4 = vec4( 0.06943, 0.33001, 0.66999, 0.93057 );
const vec4 w4 = vec4( 0.34786, 0.65215, 0.65215, 0.34786 );

float ilerp(float a, float b, float x)
{
	return clamp((x - a) / (b - a), 0.0, 1.0);
}

vec3 sqr(vec3 x)
{
	return x * x;
}

vec4 expc(vec4 x) { return exp(clamp(x, -20.0, 20.0)); }
vec3 expc(vec3 x) { return exp(clamp(x, -20.0, 20.0)); }
vec2 expc(vec2 x) { return exp(clamp(x, -20.0, 20.0)); }
float  expc(float x)  { return exp(clamp(x, -20.0, 20.0)); }


vec2 NrmToUV(vec3 vNrm)
{
	vec2 uv = vec2(dot(Const.ZeroAz, vNrm), dot(Const.SunAz, vNrm)) / Const.AngCtr;
	return (uv * 0.5 + 0.5);
}


vec3 uvToLoc(vec2 uv, float r)
{
	uv = (uv * 2.0 - 1.0);
	float w = uv.x * uv.x + uv.y * uv.y;
	if (w > 1.0f) uv *= inversesqrt(w);
	uv *= r;
	float det = 1.0f - uv.x * uv.x - uv.y * uv.y;
	float cos_ab = det > 0.0f ? sqrt(det) : 0.0f;
	return vec3(uv.xy, cos_ab);
}


vec3 uvToNrm(vec2 uv)
{
	vec3 q = uvToLoc(uv, Const.AngCtr);
	return normalize(Const.SunAz * q.y + Const.ZeroAz * q.x + Const.toCam * q.z);
}


vec3 uvToDir(vec2 uv)
{
	uv.y = uv.y * inversesqrt(0.2f + uv.y * uv.y) * sqrt(1.2f);

	float x = uv.x * 2.0f - 1.0f;
	float y = sqrt(1.0f - x * x);
	float z = 1.0f - uv.y * Const.AngRng;
	float k = sqrt(1.0f - z * z);

	return normalize(Const.SunAz * x * k + Const.ZeroAz * y * k + Const.toCam * z);
}


vec2 DirToUV(vec3 uDir)
{
	float y = dot(uDir, Const.toCam);
	vec3 uOrt = normalize(uDir - Const.toCam * y);
	float x = dot(uOrt, Const.SunAz) * 0.5 + 0.5;
	vec2 uv = vec2(x, 1.0f - (y - Const.AngMin) * Const.iAngRng);
	uv.y = sqrt(0.2f) * uv.y * inversesqrt(1.2f - uv.y * uv.y);
	return uv;
}


vec3 HDR(vec3 x)
{
	return 1.0f - exp(-Const.Expo * x);
}


vec3 LightFX(vec3 x)
{
	//return x * rsqrt(1.0f + x*x) * 1.4f;
	return 2.0 * x / (1.0f + x);
}


float RayLength(float cos_dir, float r0, float r1)
{
	float y = r0 * cos_dir;
	float z2 = r0 * r0 - y * y;
	return sqrt(r1 * r1 - z2) + y;
}


float RayLength(float cos_dir, float r0)
{
	return RayLength(cos_dir, r0, Const.AtmoRad);
}


// Compute UV and blend factor for smaple3D routine
//
vec3 TransformUV(vec3 uv, const float rc, const float prc)
{
	float ipix = 1.0f / rc;	// const: GLSL takes constant expressions only

	uv = clamp(uv, 0.0, 1.0);
	uv.z *= (rc - 1);
	uv.x *= ipix;
	uv.x += floor(uv.z) * ipix;
	return uv;
}


// Sample a 3D texture composed from an array of 2D textures
//
vec4 smaple3D(sampler2D tSamp, vec3 uv, const float rc, const float pix)
{
	float x = 1.0f / rc;	// const: GLSL takes constant expressions only

	vec4 a = texture(tSamp, uv.xy).rgba;
	vec4 b = texture(tSamp, uv.xy + vec2(x, 0)).rgba;
	return mix(a, b, fract(uv.z));
}






// Optical depth integral in atmosphere for a given distance
//
vec2 Gauss7(float cos_dir, float r0, float dist, vec2 ih0)
{
	int i;
	float x = 2.0 * r0 * cos_dir;
	float r2 = r0 * r0;

	// Compute altitudes of sample points
	vec4 d0 = dist * n0;
	vec4 a0 = sqrt(r2 + d0 * (d0 - x)) - Const.PlanetRad;
	vec4 d1 = dist * n1;
	vec4 a1 = sqrt(r2 + d1 * (d1 - x)) - Const.PlanetRad;
	
	vec2 sum = vec2(0.0f);
	for (i = 0; i < 4; i++) sum += expc(-a0[i] * ih0) * w0[i];
	for (i = 0; i < 3; i++) sum += expc(-a1[i] * ih0) * w1[i];
	return sum * dist * 0.5f;
}


// Optical depth integral in atmosphere for a given distance
//
vec2 Gauss4(float cos_dir, float r0, float dist, vec2 ih0)
{
	vec4 d0 = dist * n4;
	vec4 a0 = sqrt(r0 * r0 + d0 * d0 - 2.0 * r0 * d0 * cos_dir) - Const.PlanetRad;
	vec4 ray = expc(-a0 * ih0.x);
	vec4 mie = expc(-a0 * ih0.y);
	return vec2(dot(ray, w4), dot(mie, w4)) * dist * 0.5f;
}


// Rayleigh phase function
//
float RayPhase(float cw)
{
	return 0.25f * (4.0f + cw * cw);
}


// Henyey-Greenstein Phase function
//
/*float MiePhase(float cw)
{
	float cw2 = cw * cw;
	return Const.HG.x * (1.0f + cw2) * pow(abs(Const.HG.y - Const.HG.z * cw2*cw), -1.5f) + Const.HG.w;
}*/

float MiePhase(float cw)
{
	return 8.0f * Const.HG.x / (1.0f - Const.HG.y * cw) + Const.HG.w;
}


// Get a color of sunlight for a given altitude and normal-sun angle
//
vec3 GetSunColor(float dir, float alt)
{
	float maxalt = max(Const.MaxAlt, Const.CloudAlt);
	alt = ilerp(Const.MinAlt, maxalt, alt);
	dir = clamp((dir - MINANGLE) * iANGRNG, 0.0, 1.0);
	alt = sqrt(alt);
	return texture(tSun, vec2(dir, alt)).rgb;
}


vec3 ComputeCameraView(float a, float r, float d)
{
	vec2 rm = Gauss7(a, r, d, Const.iH) * Const.rmO;
	vec3 clr = Const.RayWave * rm.r + Const.MieWave * rm.g;
	return exp(-clr);
}



// Approximate multi-scatter effect to atmospheric color and light travel behind terminator
//
vec4 AmbientApprox(float dNS, bool bR)
{
	float fA = 1.0f - smoothstep(0.0f, Const.TW_Dst, -dNS);
	vec3 clr = (bR ? Const.RayWave : Const.cAmbient);
	return vec4(clr, fA);
}

vec4 AmbientApprox(float dNS) { return AmbientApprox(dNS, true); }	// default argument bR = true: GLSL has none

vec4 AmbientApprox(vec3 vNrm, bool bR)
{
	float dNS = dot(vNrm, Const.toSun);
	return AmbientApprox(dNS, bR);
}

vec4 AmbientApprox(vec3 vNrm) { return AmbientApprox(vNrm, true); }	// default argument bR = true: GLSL has none



struct RayData {
	float se;	// Distance to 'Shadow entry' point from a camera
	float sx;	// Shadow exit
	float ae;	// Atmosphere entry
	float ax;	// Atmosphere exit
	float hd;	// Horizon distance from a camera
	float ca;	// Closest approach distance	
};

struct IData {
	float s0, s1, e0, e1;
};


// Compute ray passage information
// vRay must point away from the camera
//
RayData ComputeRayStats(in vec3 vRay, in bool bPreProcessData)
{
	RayData dat = RayData(0.0, 0.0, 0.0, 0.0, 0.0, 0.0);

	const float invalid = -1e9;

	// Projection of viewing ray on 'shadow' axes
	float u = dot(vRay, Const.Up);
	float t = dot(vRay, Const.ZeroAz);
	float z = dot(vRay, Const.toSun);

	// Shadow Entry and Exit points
	// Cosine 'a'
	float a = u * inversesqrt(u * u + t * t);

	float k2 = Const.Cr2 * a * a;
	float h2 = Const.Cr2 - k2;
	float w2 = Const.PlanetRad2 - h2;
	vec2 b = sqrt(vec2(w2, k2));
	float k  = b.y * sign(a);
	float v2 = 0;
	float m  = Const.CamRad2 - Const.PlanetRad2;

	dat.se = k - b.x;
	dat.sx = dat.se + 2.0f * b.x;
	
	// Project distances back to 3D space
	float q = inversesqrt(max(2.5e-5, 1.0 - z * z));
	dat.se *= q;
	dat.sx *= q;

	// Compute atmosphere entry and exit points 
	//
	a = -dot(Const.toCam, vRay);
	k2 = Const.CamRad2 * a * a;
	h2 = Const.CamRad2 - k2;
	v2 = Const.AtmoRad2 - h2;
	vec3 n = sqrt(vec3(v2, k2, m));
	k = n.y * sign(a);

	dat.hd  = m > 0.0 ? n.z : 0.0;
	dat.ae = (k - n.x);
	dat.ax = dat.ae + 2.0f * n.x;
	dat.ca = Const.CamRad * a;

	// If the ray doesn't intersect atmosphere then set both distances to zero
	if (v2 < 0) dat.ae = dat.ax = invalid;

	// If the ray doesn't intersect shadow then set both distances to atmo exit
	if (w2 < 0) dat.se = dat.sx = invalid;

	if (bPreProcessData)
	{
		vec3 vEn = Const.CamPos + vRay * dat.se;
		vec3 vEx = Const.CamPos + vRay * dat.sx;

		// If shadow entry/exit point is Lit then set it to atmo exit point
		if (dot(vEn, Const.toSun) > 0) dat.se = invalid;
		if (dot(vEx, Const.toSun) > 0) dat.sx = invalid;	
	}

	return dat;
}


IData PostProcessData(RayData sp)
{
	IData d;
	if (!Flo.bCamLit) {	// Camera in Shadow
		d.s0 = max(sp.sx, sp.ae);

		float lf = max(0.0, sp.ca) / max(1.0f, abs(sp.hd)); // Lerp Factor
		float mp = mix((sp.ax + d.s0) * 0.5f, sp.hd, clamp(lf, 0.0, 1.0));

		d.e0 = mp;
		d.s1 = max(sp.sx, mp);
		d.e1 = sp.ax;
	}
	else { // Camera is Lit
		d.s0 = max(0.0, sp.ae);

		float lf = max(0.0, sp.ca) / max(1.0f, abs(sp.hd)); // Lerp Factor
		float mp = mix((sp.ax + d.s0) * 0.5f, sp.hd, clamp(lf, 0.0, 1.0));

		bool bA = (sp.se > sp.ax || sp.se < 0);
		
		d.e0 = bA ? mp : sp.se;
		d.s1 = bA ? mp : max(sp.sx, sp.ae);
		d.e1 = sp.ax;
	}
	return d;
}

// Compute attennuation from vPos in atmosphere to camera (or atm exit point)
//
vec3 ComputeCameraView(vec3 vPos, vec3 vNrm, vec3 vRay, float r)
{
	float d;
	float a = dot(vNrm, vRay);
	if (Flo.bCamInSpace) d = RayLength(a, r);
	else d = dot(vPos - Const.CamPos, vRay);
	vec2 rm = Gauss7(a, r, d, Const.iH) * Const.rmO;
	vec3 clr = Const.RayWave * rm.r + Const.MieWave * rm.g;
	return exp(-clr);
}

// Integrate viewing ray for incatter color (.rgb) and optical depth (.a)
// vRay must point from camera to vOrig
//
vec4 IntegrateSegmentMP(vec3 vOrig, vec3 vRay, float len, float iH)
{
	vec4 ret = vec4(0);
	vec3 vR = vRay * len;
	for (int i = 0; i < NSEG; i++)
	{
		vec3 pos = vOrig + vR * (iNSEG * (float(i) + 0.5f));
		vec3 n = normalize(pos);
		float rad = dot(n, pos);
		float alt = rad - Const.PlanetRad;
		vec3 x = GetSunColor(dot(n, Const.toSun), alt);
		x *= ComputeCameraView(pos, n, vRay, rad);
		float f = exp(-alt * iH) * iNSEG;
		ret.rgb += x * f;
		ret.a += f;
	}
	return ret * len;
}

vec4 IntegrateSegmentNS(vec3 vOrig, vec3 vRay, float len, float iH)
{
	// TODO: Could try to accummulation of CamView seg. by seg.

	vec4 ret = vec4(0);
	vec3 vR = vRay * len;
	for (int i = 0; i < NSEG; i++)
	{
		vec3 pos = vOrig + vR * n[i];
		vec3 n = normalize(pos);
		float rad = dot(n, pos);
		float alt = rad - Const.PlanetRad;
		vec3 x = GetSunColor(dot(n, Const.toSun), alt);
		x *= ComputeCameraView(pos, n, vRay, rad);
		float f = exp(-alt * iH) * w[i];
		ret.rgb += x * f;
		ret.a += f;
	}
	return ret * len;
}



// 2D lookup-table, for direct sunlight being filtered by atmosphere
//
vec4 SunColor(float x, float y)	// x : TEXCOORD0, y : TEXCOORD1
{
	float maxalt = max(Const.MaxAlt, Const.CloudAlt);
	float alt = mix(Const.MinAlt, maxalt, y*y);
	float ang = x * ANGRNG + MINANGLE;
	float rad = alt + Const.PlanetRad;
	float dist = RayLength(-ang, rad);
	vec2 rm = Gauss7(-ang, rad, dist, Const.iH) * Const.rmO;
	vec3 clr = Const.RayWave * rm.r + Const.MieWave * rm.g;

	return vec4(exp(-clr), 1.0f);
}

#ifdef PS_SunColor
layout(location = 0) in float iX;
layout(location = 1) in float iY;
layout(location = 0) out vec4 oColor;
void main() { oColor = SunColor(iX, iY); }
#endif


struct SkyOut
{
	vec4 ray;
	vec4 mie;
};


// Get a precomputed rayleight and mie color values for a given direction from a camera
//
SkyOut GetSkyColor(vec3 uDir)
{
	vec2 uv = DirToUV(uDir);

	SkyOut o;
	o.ray = texture(tSkyRayColor, uv).rgba;
	o.mie = texture(tSkyMieColor, uv).rgba;
	return o;
}



vec4 SkyView(float u, float v)	// u : TEXCOORD0, v : TEXCOORD1
{
	// Viewing ray
	vec3 vRay = uvToDir(vec2(u,v));

	float rmO = Flo.bRay ? Const.rmO.r : Const.rmO.g;
	float rmI = Flo.bRay ? Const.rmI.r : Const.rmI.g;
	float iH = Flo.bRay ? Const.iH.r : Const.iH.g;

	RayData sp = ComputeRayStats(vRay, true);
	IData id = PostProcessData(sp);

	// First segment
	vec4 ret = vec4(0);
	if (id.e0 > id.s0)
		ret += IntegrateSegmentNS(Const.CamPos + vRay * id.s0, vRay, id.e0 - id.s0, iH);

	// Second segment
	if (id.e1 > id.s1)
		ret += IntegrateSegmentNS(Const.CamPos + vRay * id.s1, vRay, id.e1 - id.s1, iH);


	ret.rgb *= rmI;
	ret.rgb *= Flo.bRay ? Const.RayWave : Const.MieWave;
	ret.rgb *= Const.cSun;

	if (Flo.bRay)
	{
		vec2 vDepth = Gauss7(dot(Const.toCam, -vRay), Const.CamRad, sp.ax, Const.iH);
		vec2 vOut = vDepth * Const.rmO;
		vec4 cMlt = AmbientApprox(Const.toCam);
		cMlt.rgb *= exp(-Const.CamAlt * Const.iH.r);
		cMlt.rgb *= cMlt.a;
		cMlt.rgb *= exp(-(Const.RayWave * vOut.r + Const.MieWave * vOut.g));
		float alpha = ilerp(10e3, 150e3, vDepth.r);
		return vec4(ret.rgb + cMlt.rgb, alpha);
	}

	return vec4(ret.rgb, 1.0f);
}

#ifdef PS_SkyView
layout(location = 0) in float iX;
layout(location = 1) in float iY;
layout(location = 0) out vec4 oColor;
void main() { oColor = SkyView(iX, iY); }
#endif



// 2D lookup-table for pre-computed sky color. (pre frame), varies with camera pos and sun pos
//
vec4 RingView(float u, float v)	// u : TEXCOORD0, v : TEXCOORD1
{
	if (v >= LastLine) return vec4(0, 0, 0, 0);

	v *= v;
	float rmO = Flo.bRay ? Const.rmO.r : Const.rmO.g;
	float rmI = Flo.bRay ? Const.rmI.r : Const.rmI.g;
	float iH  = Flo.bRay ? Const.iH.r : Const.iH.g;

	float x = -1.0f + u * 2.0f;
	float y = sqrt(max(1e-6, 1.0f - x * x));
	float e = v * Const.AtmoAlt;
	float re = Const.PlanetRad + e;
	float z = re * Const.CosAlpha;
	float j = re * Const.SinAlpha;

	// Sample position and viewing ray
	vec3 vPos = Const.SunAz * x * j + Const.ZeroAz * y * j + Const.toCam * z;
	vec3 vRay = normalize(vPos - Const.CamPos);
	float cpd = abs(dot(vRay, Const.CamPos - vPos));

	RayData sp = ComputeRayStats(vRay, true);
	IData id = PostProcessData(sp);

	// First segment
	vec4 ret = vec4(0);
	if (id.e0 > id.s0)
		ret += IntegrateSegmentNS(vPos - vRay * (cpd - id.s0), vRay, id.e0 - id.s0, iH);

	// Second segment
	if (id.e1 > id.s1)
		ret += IntegrateSegmentNS(vPos - vRay * (cpd - id.s1), vRay, id.e1 - id.s1, iH);

	ret.rgb *= rmI;
	ret.rgb *= Flo.bRay ? Const.RayWave : Const.MieWave;
	ret.rgb *= Const.cSun;

	if (Flo.bRay)
	{
		vec3 vSrc = vPos - vRay * (cpd - sp.ax);
		vec3 vNrm = normalize(vSrc);
		vec2 vDepth = Gauss4(dot(vNrm, vRay), dot(vSrc, vNrm), sp.ax - sp.ae, Const.iH);
		vec2 vOut = vDepth * Const.rmO;
		vec4 cMlt = AmbientApprox(Const.toCam);
		cMlt.rgb *= exp(-Const.CamAlt * Const.iH.r);
		cMlt.rgb *= cMlt.a;
		cMlt.rgb *= exp(-(Const.RayWave * vOut.r + Const.MieWave * vOut.g));
		float alpha = ilerp(10e3, 150e3, vDepth.r);
		alpha = alpha > 0 ? sqrt(alpha) : 0.0;
		return vec4(ret.rgb + cMlt.rgb, alpha);
	}

	return vec4(ret.rgb, 1.0f);
}

#ifdef PS_RingView
layout(location = 0) in float iX;
layout(location = 1) in float iY;
layout(location = 0) out vec4 oColor;
void main() { oColor = RingView(iX, iY); }
#endif


// Get a precomputed total (combined) sky color for a given direction from a camera
//
vec3 GetAmbient(vec3 vRay)
{
	return texture(tAmbient, DirToUV(vRay)).rgb;
}


// 2D lookup-table for pre-computed total (combined) sky color. (pre frame), varies with sun/cam relation
//
vec4 AmbientSky(float u, float v)	// u : TEXCOORD0, v : TEXCOORD1
{
	// Viewing ray
	vec2 uv = vec2(u, v);
	vec3 uDir = uvToDir(uv);
	float  ph = dot(uDir, Const.toSun);
	
	vec3 ray = texture(tSkyRayColor, uv).rgb;
	vec3 mie = texture(tSkyMieColor, uv).rgb;
	vec3 color = ray * RayPhase(ph) + mie * MiePhase(ph);

	return vec4(color, 1.0f);
}

#ifdef PS_AmbientSky
layout(location = 0) in float iX;
layout(location = 1) in float iY;
layout(location = 0) out vec4 oColor;
void main() { oColor = AmbientSky(iX, iY); }
#endif



struct LandOut
{
	vec4 ray;
	vec4 mie;
	vec4 atn;
};


// Get a precomputed rayleight and mie haze values for a given altitude and normal
//
LandOut GetLandView(float rad, vec3 vNrm)
{
	vec2 uv = NrmToUV(vNrm);

	float a = rad - Const.PlanetRad;
	float z = clamp((a - Const.MinAlt) * Const.iAltRng, 0.0, 1.0); // inverse lerp

	vec3 uvb = TransformUV(vec3(uv, z), Nc, Wc);

	LandOut o;
	o.ray = smaple3D(tLndRay, uvb, Nc, Wc);
	o.mie = smaple3D(tLndMie, uvb, Nc, Wc);
	o.atn = smaple3D(tLndAtn, uvb, Nc, Wc);
	return o;
}


// Render 3D Lookup texture for land view
//
vec4 LandView(float u, float v)	// u : TEXCOORD0, v : TEXCOORD1
{
	u *= Nc;
	float a = floor(u) / Nc;
	float alt = mix(Const.MinAlt, Const.MaxAlt, a);
	float r = alt + Const.PlanetRad;

	// Geo-centric Vertex location
	vec3 vNrm = uvToNrm(vec2(fract(u), v));
	vec3 vVrt = vNrm * r;

	// Viewing ray
	vec3 vCam = vVrt - Const.CamPos;
	vec3 vRay = normalize(vCam); // From camera to vertex
	float cpd = abs(dot(vRay, vCam)); // Camera pixel distance
	
	RayData sp = ComputeRayStats(vRay, true);

	float s0 = sp.ae > 0 ? sp.ae : 0.0;
	float e0 = sp.se > 0 ? min(cpd, sp.se) : cpd;

	//if (!Flo.bRay) dist = min(dist, 10e3);

	float rmI = Flo.bRay ? Const.rmI.r : Const.rmI.g;
	float iH  = Flo.bRay ? Const.iH.r : Const.iH.g;

	vec4 ret = IntegrateSegmentNS(vVrt - vRay * (cpd - s0), vRay, e0 - s0, iH);

	ret.rgb *= Const.cSun;
	ret.rgb *= rmI;
	ret.rgb *= Flo.bRay ? Const.RayWave : Const.MieWave;

	return vec4(ret.rgb, 0.0f);
}

#ifdef PS_LandView
layout(location = 0) in float iX;
layout(location = 1) in float iY;
layout(location = 0) out vec4 oColor;
void main() { oColor = LandView(iX, iY); }
#endif



// Render 3D Lookup texture for land view attennuation
//
vec4 LandViewAtten(float u, float v)	// u : TEXCOORD0, v : TEXCOORD1
{
	u *= Nc;
	float a = floor(u) / Nc;
	float alt = mix(Const.MinAlt, Const.MaxAlt, a);
	float r = alt + Const.PlanetRad;

	// Geo-centric Vertex location
	vec3 vNrm = uvToNrm(vec2(fract(u), v));
	vec3 vVrt = vNrm * r;

	// Viewing ray
	vec3 vCam = vVrt - Const.CamPos;
	vec3 vRay = normalize(vCam); // Towards vertex from camera

	float ang = dot(vNrm, vRay);
	float len = RayLength(ang, r);

	float dist = min(len, dot(vRay, vCam));

	vec3 ret = ComputeCameraView(ang, r, dist);

	return vec4(ret, 1.0);
}

#ifdef PS_LandViewAtten
layout(location = 0) in float iX;
layout(location = 1) in float iY;
layout(location = 0) out vec4 oColor;
void main() { oColor = LandViewAtten(iX, iY); }
#endif

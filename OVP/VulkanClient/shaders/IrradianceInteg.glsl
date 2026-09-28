

#define IKernelSize 150

layout(binding = 1, row_major, scalar) uniform IrradianceIntegPS	// uniform extern: pixel shader constants (IPI.glsl's are binding 0)
{
vec4  Kernel[IKernelSize];
vec3  vNr; // North
vec3  vUp; // Up
vec3  vCp; // Forward (East)
vec2  fD;
float   fIntensity;
bool    bUp;
};

layout(binding = 4) uniform samplerCube tCube;	// s0
layout(binding = 5) uniform sampler2D tSrc;	// s1


vec3 Paraboloidal_to_World(vec3 i)
{
	i.xy *= 1.1f;
	float  d = (1.0f - dot(i.xy, i.xy)) * 0.5f;
	vec3 p = normalize(vec3(i.xy, d));
	return (vCp*p.x) + (vNr*p.y) + (vUp * p.z * i.z);
}


vec4 PSPreInteg(float x, float y)	// x : TEXCOORD0, y : TEXCOORD1
{
	vec3 color = vec3(0);
	vec2 p = vec2(x, y);
	for (int j = 0; j < 8; j++) {
		for (int i = 0; i < 8; i++) {
			color += texture(tSrc, p + (vec2(i, j) - 4) * fD).rgb;
		}
	}
	return vec4(color * (1.0f/64.0f), 1);
}


vec4 PSInteg(float x, float y, vec2 sc)	// x : TEXCOORD0, y : TEXCOORD1, sc : VPOS
{
	vec2 qw = vec2(x, y) * 2.0f - 1.0f;
	vec3 vz = Paraboloidal_to_World(vec3(qw.xy, (bUp ? 1 : -1)));

	vec3 q = mix(vUp, vCp, fract(x * 21)); // Randomize rotation
	vec3 w = mix(q, vNr, fract(y * 17));  // Randomize rotation
	vec3 vx = normalize(cross(vz, w));
	vec3 vy = normalize(cross(vz, vx));

	vec3 sum = vec3(0);

	for (int i = 0; i < IKernelSize; i++) {
		vec3 d = (vx*Kernel[i].x) + (vy*Kernel[i].y) + (vz*Kernel[i].z);
		sum += texture(tCube, d).rgb * Kernel[i].w;
	}
	
	return vec4(sqrt(sum * fIntensity * (0.7f / IKernelSize)), 1.0f);
}


vec4 PSPostBlur(float x, float y)	// x : TEXCOORD0, y : TEXCOORD1
{
	vec3 color = vec3(0);
	vec2 p = vec2(x, y);
	color += texture(tSrc, p).rgb;
	color += texture(tSrc, p + vec2(0,  1)*fD).rgb;
	color += texture(tSrc, p + vec2(0, -1)*fD).rgb;
	color += texture(tSrc, p + vec2(-1, 0)*fD).rgb;
	color += texture(tSrc, p + vec2(-1, 0)*fD).rgb;
	return vec4(color * 0.2, 1);
}

#if defined(PS_PSPreInteg) || defined(PS_PSInteg) || defined(PS_PSPostBlur)
layout(location = 0) in float iX;	// TEXCOORD0 of IPI.glsl
layout(location = 1) in float iY;	// TEXCOORD1
layout(location = 0) out vec4 oColor;
#endif
#ifdef PS_PSPreInteg
void main() { oColor = PSPreInteg(iX, iY); }
#endif
#ifdef PS_PSInteg
void main() { oColor = PSInteg(iX, iY, gl_FragCoord.xy - 0.5); }
#endif
#ifdef PS_PSPostBlur
void main() { oColor = PSPostBlur(iX, iY); }
#endif

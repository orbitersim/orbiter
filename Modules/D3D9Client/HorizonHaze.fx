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

HazeVS HazeTechVS(HZVERTEX vrt)
{
    // Zero output.
	HazeVS outVS = (HazeVS)0;
	
    float3 posW = mul(float4(vrt.posL, 1.0f), gW).xyz;
	outVS.posH  = mul(float4(posW, 1.0f), gVP);
	outVS.tex0  = vrt.tex0;
    outVS.color = vrt.color;
    return outVS;
}


// Horizon haze pixel-shader frg.tex0.y is the altitude. 0.0 = Horizon (ground level) 1.0 = top of atmosphere
//
float4 HazeTechPS(HazeVS frg) : COLOR
{
    return frg.color * tex2D(ClampS, frg.tex0); 

    //return float4(frg.color.rgb, frg.color.a*frg.tex0.y*frg.tex0.y);
    //return float4(frg.color.rgb*(frg.tex0.y+0.30), frg.color.a*frg.tex0.y*frg.tex0.y);
}


technique HazeTech
{
    pass P0
    {
        vertexShader = compile VS_MOD HazeTechVS();
        pixelShader  = compile PS_MOD HazeTechPS();

        AlphaBlendEnable = true;
        BlendOp = Add;
        SrcBlend = SrcAlpha;
        DestBlend = InvSrcAlpha;
        ZEnable = false;
        ZWriteEnable = false;
    }
}
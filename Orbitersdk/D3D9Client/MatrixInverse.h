/*
 * SGI FREE SOFTWARE LICENSE B (Version 2.0, Sept. 18, 2008)
 * Copyright (C) 1991-2000 Silicon Graphics, Inc. All Rights Reserved.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice including the dates of first publication and
 * either this permission notice or a reference to
 * http://oss.sgi.com/projects/FreeB/
 * shall be included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * SILICON GRAPHICS, INC. BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 * WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF
 * OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 * Except as contained in this notice, the name of Silicon Graphics, Inc.
 * shall not be used in advertising or otherwise to promote the sale, use or
 * other dealings in this Software without prior written authorization from
 * Silicon Graphics, Inc.
 */

/*
** Invert 4x4 matrix.
** Contributed by David Moore (See Mesa bug #6748) (glu/src/libutil/project.c)
** Modified for MATRIX4 by Jarmo Nikkanen
*/
inline void MATRIX4Inverse(MATRIX4 &invOut, double *_det, MATRIX4 &m)
{
    double inv[16], det;
   
    inv[0] =   m.data[5]*m.data[10]*m.data[15] - m.data[5]*m.data[11]*m.data[14] - m.data[9]*m.data[6]*m.data[15]
              +m.data[9]*m.data[7]*m.data[14]  + m.data[13]*m.data[6]*m.data[11] - m.data[13]*m.data[7]*m.data[10];
    inv[4] =  -m.data[4]*m.data[10]*m.data[15] + m.data[4]*m.data[11]*m.data[14] + m.data[8]*m.data[6]*m.data[15]
              -m.data[8]*m.data[7]*m.data[14]  - m.data[12]*m.data[6]*m.data[11] + m.data[12]*m.data[7]*m.data[10];
    inv[8] =   m.data[4]*m.data[9]*m.data[15]  - m.data[4]*m.data[11]*m.data[13] - m.data[8]*m.data[5]*m.data[15]
              +m.data[8]*m.data[7]*m.data[13]  + m.data[12]*m.data[5]*m.data[11] - m.data[12]*m.data[7]*m.data[9];
    inv[12] = -m.data[4]*m.data[9]*m.data[14]  + m.data[4]*m.data[10]*m.data[13] + m.data[8]*m.data[5]*m.data[14]
              -m.data[8]*m.data[6]*m.data[13]  - m.data[12]*m.data[5]*m.data[10] + m.data[12]*m.data[6]*m.data[9];
    inv[1] =  -m.data[1]*m.data[10]*m.data[15] + m.data[1]*m.data[11]*m.data[14] + m.data[9]*m.data[2]*m.data[15]
              -m.data[9]*m.data[3]*m.data[14]  - m.data[13]*m.data[2]*m.data[11] + m.data[13]*m.data[3]*m.data[10];
    inv[5] =   m.data[0]*m.data[10]*m.data[15] - m.data[0]*m.data[11]*m.data[14] - m.data[8]*m.data[2]*m.data[15]
              +m.data[8]*m.data[3]*m.data[14]  + m.data[12]*m.data[2]*m.data[11] - m.data[12]*m.data[3]*m.data[10];
    inv[9] =  -m.data[0]*m.data[9]*m.data[15]  + m.data[0]*m.data[11]*m.data[13] + m.data[8]*m.data[1]*m.data[15]
              -m.data[8]*m.data[3]*m.data[13]  - m.data[12]*m.data[1]*m.data[11] + m.data[12]*m.data[3]*m.data[9];
    inv[13] =  m.data[0]*m.data[9]*m.data[14]  - m.data[0]*m.data[10]*m.data[13] - m.data[8]*m.data[1]*m.data[14]
              +m.data[8]*m.data[2]*m.data[13]  + m.data[12]*m.data[1]*m.data[10] - m.data[12]*m.data[2]*m.data[9];
    inv[2] =   m.data[1]*m.data[6]*m.data[15]  - m.data[1]*m.data[7]*m.data[14]  - m.data[5]*m.data[2]*m.data[15]
              +m.data[5]*m.data[3]*m.data[14]  + m.data[13]*m.data[2]*m.data[7]  - m.data[13]*m.data[3]*m.data[6];
    inv[6] =  -m.data[0]*m.data[6]*m.data[15]  + m.data[0]*m.data[7]*m.data[14]  + m.data[4]*m.data[2]*m.data[15]
              -m.data[4]*m.data[3]*m.data[14]  - m.data[12]*m.data[2]*m.data[7]  + m.data[12]*m.data[3]*m.data[6];
    inv[10] =  m.data[0]*m.data[5]*m.data[15]  - m.data[0]*m.data[7]*m.data[13]  - m.data[4]*m.data[1]*m.data[15]
              +m.data[4]*m.data[3]*m.data[13]  + m.data[12]*m.data[1]*m.data[7]  - m.data[12]*m.data[3]*m.data[5];
    inv[14] = -m.data[0]*m.data[5]*m.data[14]  + m.data[0]*m.data[6]*m.data[13]  + m.data[4]*m.data[1]*m.data[14]
              -m.data[4]*m.data[2]*m.data[13]  - m.data[12]*m.data[1]*m.data[6]  + m.data[12]*m.data[2]*m.data[5];
    inv[3] =  -m.data[1]*m.data[6]*m.data[11]  + m.data[1]*m.data[7]*m.data[10]  + m.data[5]*m.data[2]*m.data[11]
              -m.data[5]*m.data[3]*m.data[10]  - m.data[9]*m.data[2]*m.data[7]   + m.data[9]*m.data[3]*m.data[6];
    inv[7] =   m.data[0]*m.data[6]*m.data[11]  - m.data[0]*m.data[7]*m.data[10]  - m.data[4]*m.data[2]*m.data[11]
              +m.data[4]*m.data[3]*m.data[10]  + m.data[8]*m.data[2]*m.data[7]   - m.data[8]*m.data[3]*m.data[6];
    inv[11] = -m.data[0]*m.data[5]*m.data[11]  + m.data[0]*m.data[7]*m.data[9]   + m.data[4]*m.data[1]*m.data[11]
              -m.data[4]*m.data[3]*m.data[9]   - m.data[8]*m.data[1]*m.data[7]   + m.data[8]*m.data[3]*m.data[5];
    inv[15] =  m.data[0]*m.data[5]*m.data[10]  - m.data[0]*m.data[6]*m.data[9]   - m.data[4]*m.data[1]*m.data[10]
              +m.data[4]*m.data[2]*m.data[9]   + m.data[8]*m.data[1]*m.data[6]   - m.data[8]*m.data[2]*m.data[5];

    det = m.data[0]*inv[0] + m.data[1]*inv[4] + m.data[2]*inv[8] + m.data[3]*inv[12];

	if (det == 0) {
		for (int i = 0; i < 16; i++) invOut.data[i] = 0.0;
		if (_det) *_det = det;
		return;
	}
	
	if (_det) *_det = det;

    det = 1.0 / det;

    for (int i = 0; i < 16; i++) invOut.data[i] = inv[i] * det;
}
/*
 *  Copyright (C) 1995-1999 Microsoft Corporation. All Rights Reserved.
 *
 *  File: d3dmacs.h
 *
 *  Useful macros for generating execute buffers.  Consult the D3D sample
 *  code for examples of their usage.
 *
 *  Use OP_NOP to QWORD align triangle and line instructions.
 */

#ifndef __D3DMACS_H__
#define __D3DMACS_H__

#undef RELEASE

#ifndef __cplusplus
#define MAKE_MATRIX(lpDev, handle, data) \
{   if (lpDev->lpVtbl->CreateMatrix(lpDev, &handle) != D3D_OK) \
    	return FALSE; \
    if (lpDev->lpVtbl->SetMatrix(lpDev, handle, &data) != D3D_OK) \
    	return FALSE; \
}
#define RELEASE(x) {if (x != NULL) {x->lpVtbl->Release(x); x = NULL;}}
#endif

#ifdef __cplusplus
#define MAKE_MATRIX(lpDev, handle, data) \
{   if (lpDev->CreateMatrix(&handle) != D3D_OK) \
    	return FALSE; \
    if (lpDev->SetMatrix(handle, &data) != D3D_OK) \
    	return FALSE; \
}
#define RELEASE(x) {if (x != NULL) {x->Release(); x = NULL;}}
#endif

#define PUTD3DINSTRUCTION(op, sz, cnt, ptr) \
(   ((LPD3DINSTRUCTION) ptr)->bOpcode = op, \
    ((LPD3DINSTRUCTION) ptr)->bSize = sz, \
    ((LPD3DINSTRUCTION) ptr)->wCount = cnt, \
    ptr = (void *)(((LPD3DINSTRUCTION) ptr) + 1) \
)

#define VERTEX_DATA(loc, cnt, ptr) \
(   (((ptr) != (loc))? memcpy((ptr), (loc), sizeof(D3DVERTEX) * (cnt)) : 0), \
    ptr = (void *)(((LPD3DVERTEX) (ptr)) + (cnt)) \
)

// OP_MATRIX_MULTIPLY size: 4 (sizeof D3DINSTRUCTION)
#define OP_MATRIX_MULTIPLY(cnt, ptr) \
    PUTD3DINSTRUCTION(D3DOP_MATRIXMULTIPLY, sizeof(D3DMATRIXMULTIPLY), cnt, ptr)

// MATRIX_MULTIPLY_DATA size: 12 (sizeof MATRIXMULTIPLY)
#define MATRIX_MULTIPLY_DATA(src1, src2, dest, ptr) \
(   ((LPD3DMATRIXMULTIPLY) ptr)->hSrcMatrix1 = src1, \
    ((LPD3DMATRIXMULTIPLY) ptr)->hSrcMatrix2 = src2, \
    ((LPD3DMATRIXMULTIPLY) ptr)->hDestMatrix = dest, \
    ptr = (void *)(((LPD3DMATRIXMULTIPLY) ptr) + 1) \
)

// OP_STATE_LIGHT size: 4 (sizeof D3DINSTRUCTION)
#define OP_STATE_LIGHT(cnt, ptr) \
    PUTD3DINSTRUCTION(D3DOP_STATELIGHT, sizeof(D3DSTATE), cnt, ptr)

// OP_STATE_TRANSFORM size: 4 (sizeof D3DINSTRUCTION)
#define OP_STATE_TRANSFORM(cnt, ptr) \
    PUTD3DINSTRUCTION(D3DOP_STATETRANSFORM, sizeof(D3DSTATE), cnt, ptr)

// OP_STATE_RENDER size: 4 (sizeof D3DINSTRUCTION)
#define OP_STATE_RENDER(cnt, ptr) \
    PUTD3DINSTRUCTION(D3DOP_STATERENDER, sizeof(D3DSTATE), cnt, ptr)

// STATE_DATA size: 8 (sizeof D3DSTATE)
#define STATE_DATA(type, arg, ptr) \
(   ((LPD3DSTATE) ptr)->drstRenderStateType = (D3DRENDERSTATETYPE)type, \
    ((LPD3DSTATE) ptr)->dwArg[0] = arg, \
    ptr = (void *)(((LPD3DSTATE) ptr) + 1) \
)

// OP_PROCESS_VERTICES size: 4 (sizeof D3DINSTRUCTION)
#define OP_PROCESS_VERTICES(cnt, ptr) \
    PUTD3DINSTRUCTION(D3DOP_PROCESSVERTICES, sizeof(D3DPROCESSVERTICES), cnt, ptr)

// PROCESSVERTICES_DATA size: 16 (sizeof D3DPROCESSVERTICES)
#define PROCESSVERTICES_DATA(flgs, strt, cnt, ptr) \
(   ((LPD3DPROCESSVERTICES) ptr)->dwFlags = flgs, \
    ((LPD3DPROCESSVERTICES) ptr)->wStart = strt, \
    ((LPD3DPROCESSVERTICES) ptr)->wDest = strt, \
    ((LPD3DPROCESSVERTICES) ptr)->dwCount = cnt, \
    ((LPD3DPROCESSVERTICES) ptr)->dwReserved = 0, \
    ptr = (void *)(((LPD3DPROCESSVERTICES) ptr) + 1) \
)

// OP_TRIANGLE_LIST size: 4 (sizeof D3DINSTRUCTION)
#define OP_TRIANGLE_LIST(cnt, ptr) \
    PUTD3DINSTRUCTION(D3DOP_TRIANGLE, sizeof(D3DTRIANGLE), cnt, ptr)

#define TRIANGLE_LIST_DATA(loc, count, ptr) \
(   ((ptr) != (loc))? memcpy((ptr), (loc), sizeof(D3DTRIANGLE) * (count)) : 0, \
    ptr = (void *)(((LPD3DTRIANGLE) (ptr)) + (count)) \
)

// OP_TRIANGLEEX_LIST size: 4 (sizeof D3DINSTRUCTION)
#define OP_TRIANGLEEX_LIST(cnt, ptr) \
    PUTD3DINSTRUCTION(D3DOP_TRIANGLEEX, sizeof(D3DTRIANGLEEX), cnt, ptr)

#define TRIANGLEEX_LIST_DATA(loc, count, ptr) \
(   ((ptr) != (loc))? memcpy((ptr), (loc), sizeof(D3DTRIANGLEEX) * (count)) : 0, \
    ptr = (void *)(((LPD3DTRIANGLEEX) (ptr)) + (count)) \
)

// OP_LINE_LIST size: 4 (sizeof D3DINSTRUCTION)
#define OP_LINE_LIST(cnt, ptr) \
    PUTD3DINSTRUCTION(D3DOP_LINE, sizeof(D3DLINE), cnt, ptr)

#define LINE_LIST_DATA(loc, count, ptr) \
(   ((ptr) != (loc))? memcpy((ptr), (loc), sizeof(D3DLINE) * (count)) : 0, \
    ptr = (void *)(((LPD3DLINE) (ptr)) + (count)) \
)

// OP_POINT_LIST size: 8 (sizeof D3DINSTRUCTION + sizeof D3DPOINT)
#define OP_POINT_LIST(first, cnt, ptr) \
(   PUTD3DINSTRUCTION(D3DOP_POINT, sizeof(D3DPOINT), 1, ptr), \
    ((LPD3DPOINT)(ptr))->wCount = cnt, \
    ((LPD3DPOINT)(ptr))->wFirst = first, \
    ptr = (void*)(((LPD3DPOINT)(ptr)) + 1) \
)

// OP_SPAN_LIST size: 8 (sizeof D3DINSTRUCTION + sizeof D3DSPAN)
#define OP_SPAN_LIST(first, cnt, ptr) \
(   PUTD3DINSTRUCTION(D3DOP_SPAN, sizeof(D3DSPAN), 1, ptr), \
    ((LPD3DSPAN)(ptr))->wCount = cnt, \
    ((LPD3DSPAN)(ptr))->wFirst = first, \
    ptr = (void*)(((LPD3DSPAN)(ptr)) + 1) \
)

// OP_BRANCH_FORWARD size: 18 (sizeof D3DINSTRUCTION + sizeof D3DBRANCH)
#define OP_BRANCH_FORWARD(tmask, tvalue, tnegate, toffset, ptr) \
(   PUTD3DINSTRUCTION(D3DOP_BRANCHFORWARD, sizeof(D3DBRANCH), 1, ptr), \
    ((LPD3DBRANCH) ptr)->dwMask = tmask, \
    ((LPD3DBRANCH) ptr)->dwValue = tvalue, \
    ((LPD3DBRANCH) ptr)->bNegate = tnegate, \
    ((LPD3DBRANCH) ptr)->dwOffset = toffset, \
    ptr = (void *)(((LPD3DBRANCH) (ptr)) + 1) \
)

// OP_SET_STATUS size: 20 (sizeof D3DINSTRUCTION + sizeof D3DSTATUS)
#define OP_SET_STATUS(flags, status, _x1, _y1, _x2, _y2, ptr) \
(   PUTD3DINSTRUCTION(D3DOP_SETSTATUS, sizeof(D3DSTATUS), 1, ptr), \
    ((LPD3DSTATUS)(ptr))->dwFlags = flags, \
    ((LPD3DSTATUS)(ptr))->dwStatus = status, \
    ((LPD3DSTATUS)(ptr))->drExtent.x1 = _x1, \
    ((LPD3DSTATUS)(ptr))->drExtent.y1 = _y1, \
    ((LPD3DSTATUS)(ptr))->drExtent.x2 = _x2, \
    ((LPD3DSTATUS)(ptr))->drExtent.y2 = _y2, \
    ptr = (void *)(((LPD3DSTATUS) (ptr)) + 1) \
)

// OP_NOP size: 4
#define OP_NOP(ptr) \
    PUTD3DINSTRUCTION(D3DOP_TRIANGLE, sizeof(D3DTRIANGLE), 0, ptr)

#define OP_EXIT(ptr) \
    PUTD3DINSTRUCTION(D3DOP_EXIT, 0, 0, ptr)

#define QWORD_ALIGNED(ptr) \
    !(0x00000007L & (ULONG)(ptr))

#endif // __D3DMACS_H__

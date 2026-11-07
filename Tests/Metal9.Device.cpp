#include <catch2/catch_test_macros.hpp>
#include "d3dx9.h"
#include "OrbiterPlatform.h"
#include <vector>

namespace {

struct Vtx { float x, y, z, u, v; };

const D3DVERTEXELEMENT9 kDecl[] = {
	{0, 0, D3DDECLTYPE_FLOAT3, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_POSITION, 0},
	{0, 12, D3DDECLTYPE_FLOAT2, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_TEXCOORD, 0},
	D3DDECL_END()
};

DWORD ReadPixel(IDirect3DDevice9 *dev, IDirect3DSurface9 *rt, int x, int y)
{
	D3DSURFACE_DESC d;
	rt->GetDesc(&d);
	IDirect3DSurface9 *sys = nullptr;
	REQUIRE(dev->CreateOffscreenPlainSurface(d.Width, d.Height, d.Format, D3DPOOL_SYSTEMMEM, &sys, nullptr) == S_OK);
	REQUIRE(dev->GetRenderTargetData(rt, sys) == S_OK);
	D3DLOCKED_RECT lr;
	REQUIRE(sys->LockRect(&lr, nullptr, D3DLOCK_READONLY) == S_OK);
	DWORD v = ((DWORD *)((BYTE *)lr.pBits + y * lr.Pitch))[x];
	sys->UnlockRect();
	sys->Release();
	return v & 0xFFFFFF;
}

}

TEST_CASE("Metal9 renders effects, textures and clears", "[metal9]")
{
	oplat::Init();
	oplat::WindowDesc wd;
	wd.width = 64;
	wd.height = 64;
	wd.visible = false;
	HWND hWnd = oplat::OpenWindow(wd);
	REQUIRE(hWnd);

	IDirect3D9 *d3d = Direct3DCreate9(D3D_SDK_VERSION);
	REQUIRE(d3d);
	D3DPRESENT_PARAMETERS pp = {};
	pp.Windowed = TRUE;
	pp.BackBufferWidth = 64;
	pp.BackBufferHeight = 64;
	pp.BackBufferFormat = D3DFMT_X8R8G8B8;
	pp.EnableAutoDepthStencil = TRUE;
	pp.AutoDepthStencilFormat = D3DFMT_D24S8;
	pp.hDeviceWindow = hWnd;
	IDirect3DDevice9 *dev = nullptr;
	REQUIRE(d3d->CreateDevice(0, D3DDEVTYPE_HAL, hWnd, D3DCREATE_HARDWARE_VERTEXPROCESSING, &pp, &dev) == S_OK);

	IDirect3DTexture9 *rtex = nullptr;
	REQUIRE(dev->CreateTexture(32, 32, 1, D3DUSAGE_RENDERTARGET, D3DFMT_X8R8G8B8, D3DPOOL_DEFAULT, &rtex, nullptr) == S_OK);
	IDirect3DSurface9 *rt = nullptr;
	rtex->GetSurfaceLevel(0, &rt);

	SECTION("clear and color fill") {
		dev->SetRenderTarget(0, rt);
		REQUIRE(dev->Clear(0, nullptr, D3DCLEAR_TARGET, 0x00336699, 1.0f, 0) == S_OK);
		CHECK(ReadPixel(dev, rt, 5, 5) == 0x336699);
		RECT r = {0, 0, 8, 8};
		REQUIRE(dev->ColorFill(rt, &r, 0x00FF0000) == S_OK);
		CHECK(ReadPixel(dev, rt, 2, 2) == 0xFF0000);
		CHECK(ReadPixel(dev, rt, 20, 20) == 0x336699);
	}

	SECTION("effect draws with constants and textures") {
		ID3DXEffect *fx = nullptr;
		ID3DXBuffer *err = nullptr;
		REQUIRE(D3DXCreateEffectFromFileA(dev, "Tests/data/M9Test.fx", nullptr, nullptr, 0, nullptr, &fx, &err) == S_OK);
		IDirect3DVertexDeclaration9 *decl = nullptr;
		REQUIRE(dev->CreateVertexDeclaration(kDecl, &decl) == S_OK);

		D3DXMATRIX ident;
		D3DXMatrixIdentity(&ident);
		Vtx quad[4] = {{-1, 1, 0, 0, 0}, {1, 1, 0, 1, 0}, {-1, -1, 0, 0, 1}, {1, -1, 0, 1, 1}};

		dev->SetRenderTarget(0, rt);
		dev->Clear(0, nullptr, D3DCLEAR_TARGET, 0, 1.0f, 0);
		dev->BeginScene();
		dev->SetVertexDeclaration(decl);
		fx->SetTechnique("Solid");
		fx->SetMatrix("gW", &ident);
		D3DXVECTOR4 green(0, 1, 0, 1);
		fx->SetVector("gColor", &green);
		fx->SetBool("gUseTex", false);
		UINT n = 0;
		fx->Begin(&n, 0);
		REQUIRE(n == 1);
		fx->BeginPass(0);
		REQUIRE(dev->DrawPrimitiveUP(D3DPT_TRIANGLESTRIP, 2, quad, sizeof(Vtx)) == S_OK);
		fx->EndPass();
		fx->End();
		dev->EndScene();
		CHECK(ReadPixel(dev, rt, 16, 16) == 0x00FF00);

		IDirect3DTexture9 *tex = nullptr;
		REQUIRE(dev->CreateTexture(2, 2, 1, 0, D3DFMT_A8R8G8B8, D3DPOOL_MANAGED, &tex, nullptr) == S_OK);
		D3DLOCKED_RECT lr;
		REQUIRE(tex->LockRect(0, &lr, nullptr, 0) == S_OK);
		DWORD px[4] = {0xFFFF0000, 0xFF00FF00, 0xFF0000FF, 0xFFFFFFFF};
		for (int y = 0; y < 2; y++) memcpy((BYTE *)lr.pBits + y * lr.Pitch, px + y * 2, 8);
		tex->UnlockRect(0);

		dev->BeginScene();
		fx->SetBool("gUseTex", true);
		fx->SetTexture("gTex", tex);
		fx->Begin(&n, 0);
		fx->BeginPass(0);
		dev->DrawPrimitiveUP(D3DPT_TRIANGLESTRIP, 2, quad, sizeof(Vtx));
		fx->EndPass();
		fx->End();
		dev->EndScene();
		CHECK(ReadPixel(dev, rt, 4, 4) == 0xFF0000);
		CHECK(ReadPixel(dev, rt, 28, 4) == 0x00FF00);
		CHECK(ReadPixel(dev, rt, 4, 28) == 0x0000FF);
		CHECK(ReadPixel(dev, rt, 28, 28) == 0xFFFFFF);

		tex->Release();
		decl->Release();
		fx->Release();
	}

	rt->Release();
	rtex->Release();
	dev->Release();
	d3d->Release();
}

#include "M9Internal.h"
#include "ClientImGui.h"
#include "imgui.h"
#include "imgui_impl_metal.h"

static MTLRenderPassDescriptor *TargetPass(IDirect3DSurface9 *target)
{
	m9::Surface *s = SurfImpl(target);
	if (!s) return nil;
	MTLRenderPassDescriptor *rp = [MTLRenderPassDescriptor renderPassDescriptor];
	rp.colorAttachments[0].texture = s->tex->tex;
	rp.colorAttachments[0].level = s->level;
	rp.colorAttachments[0].slice = s->tex->type == m9::TT_CUBE ? s->face : 0;
	rp.colorAttachments[0].loadAction = MTLLoadActionLoad;
	rp.colorAttachments[0].storeAction = MTLStoreActionStore;
	return rp;
}

bool ClientImGuiInit()
{
	m9::Device *d = m9::CurrentDevice();
	return d && ImGui_ImplMetal_Init(d->mtl);
}

void ClientImGuiShutdown()
{
	m9::Device *d = m9::CurrentDevice();
	if (d) d->WaitIdle();
	ImGui_ImplMetal_Shutdown();
}

void ClientImGuiNewFrame()
{
	m9::Device *d = m9::CurrentDevice();
	if (!d || d->chains.empty()) return;
	MTLRenderPassDescriptor *rp = TargetPass(d->chains[0]->backBuffer);
	if (rp) ImGui_ImplMetal_NewFrame(rp);
}

void ClientImGuiRender(ImDrawData *data, IDirect3DSurface9 *target)
{
	m9::Device *d = m9::CurrentDevice();
	if (!d || !data) return;
	@autoreleasepool {
		MTLRenderPassDescriptor *rp = TargetPass(target);
		if (!rp) return;
		ImGui_ImplMetal_NewFrame(rp);
		d->EndEncoders();
		id<MTLCommandBuffer> cmd = d->Cmd();
		id<MTLRenderCommandEncoder> enc = [cmd renderCommandEncoderWithDescriptor:rp];
		ImGui_ImplMetal_RenderDrawData(data, cmd, enc);
		[enc endEncoding];
		d->passDirty = true;
		if (m9::Surface *s = SurfImpl(target)) if (s->tex->msaa) s->tex->msaaDirty = true;
	}
}

uint64_t ClientImGuiTexture(IDirect3DBaseTexture9 *tex)
{
	if (!tex || !tex->Impl()) return 0;
	return (uint64_t)(__bridge void *)tex->Impl()->SampleTexture();
}

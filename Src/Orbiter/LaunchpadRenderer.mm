#import <Metal/Metal.h>
#import <QuartzCore/CAMetalLayer.h>
#include "LaunchpadRenderer.h"
#include "OrbiterPlatform.h"
#include "imgui.h"
#include "imgui_impl_metal.h"

namespace orbiter {

struct LaunchpadRenderer::Impl {
	id<MTLDevice> device = nil;
	id<MTLCommandQueue> queue = nil;
	CAMetalLayer *layer = nil;
	id<CAMetalDrawable> drawable = nil;
	MTLRenderPassDescriptor *pass = nil;
	bool initialised = false;
};

LaunchpadRenderer::LaunchpadRenderer() : impl(new Impl) {}

LaunchpadRenderer::~LaunchpadRenderer()
{
	Shutdown();
	delete impl;
}

bool LaunchpadRenderer::Init(HWND hWnd)
{
	impl->layer = (__bridge CAMetalLayer *)oplat::GetMetalLayer(hWnd);
	if (!impl->layer) return false;
	impl->device = impl->layer.device ? impl->layer.device : MTLCreateSystemDefaultDevice();
	impl->layer.device = impl->device;
	impl->queue = [impl->device newCommandQueue];
	impl->pass = [MTLRenderPassDescriptor new];
	if (!ImGui_ImplMetal_Init(impl->device)) return false;
	impl->initialised = true;
	return true;
}

void LaunchpadRenderer::Shutdown()
{
	if (!impl->initialised) return;
	ImGui_ImplMetal_Shutdown();
	impl->initialised = false;
	impl->queue = nil;
	impl->layer = nil;
	impl->device = nil;
	impl->pass = nil;
}

bool LaunchpadRenderer::BeginFrame()
{
	if (!impl->initialised) return false;
	impl->drawable = [impl->layer nextDrawable];
	if (!impl->drawable) return false;
	MTLRenderPassColorAttachmentDescriptor *c = impl->pass.colorAttachments[0];
	c.texture = impl->drawable.texture;
	c.loadAction = MTLLoadActionClear;
	c.storeAction = MTLStoreActionStore;
	c.clearColor = MTLClearColorMake(0.11, 0.11, 0.12, 1.0);
	ImGui_ImplMetal_NewFrame(impl->pass);
	return true;
}

void LaunchpadRenderer::EndFrame(ImDrawData *drawData)
{
	if (!impl->drawable) return;
	@autoreleasepool {
		id<MTLCommandBuffer> cmd = [impl->queue commandBuffer];
		id<MTLRenderCommandEncoder> enc = [cmd renderCommandEncoderWithDescriptor:impl->pass];
		ImGui_ImplMetal_RenderDrawData(drawData, cmd, enc);
		[enc endEncoding];
		[cmd presentDrawable:impl->drawable];
		[cmd commit];
	}
	impl->drawable = nil;
}

void *LaunchpadRenderer::CreateTexture(const unsigned char *bgra, int width, int height)
{
	if (!impl->device || width <= 0 || height <= 0) return nullptr;
	MTLTextureDescriptor *d = [MTLTextureDescriptor texture2DDescriptorWithPixelFormat:MTLPixelFormatBGRA8Unorm
		width:width height:height mipmapped:NO];
	d.usage = MTLTextureUsageShaderRead;
	id<MTLTexture> tex = [impl->device newTextureWithDescriptor:d];
	[tex replaceRegion:MTLRegionMake2D(0, 0, width, height) mipmapLevel:0 withBytes:bgra bytesPerRow:width * 4];
	return (__bridge_retained void *)tex;
}

void LaunchpadRenderer::ReleaseTexture(void *texture)
{
	if (!texture) return;
	id<MTLTexture> tex = (__bridge_transfer id<MTLTexture>)texture;
	tex = nil;
}

}

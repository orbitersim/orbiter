#import <AppKit/AppKit.h>
#import <ImageIO/ImageIO.h>
#import <CoreGraphics/CoreGraphics.h>
#import <UniformTypeIdentifiers/UniformTypeIdentifiers.h>
#include "OrbiterPlatform.h"

namespace {

bool DecodeSource(CGImageSourceRef src, oplat::Image &img, int width, int height)
{
	if (!src) return false;
	CGImageRef cg = CGImageSourceCreateImageAtIndex(src, 0, nullptr);
	CFRelease(src);
	if (!cg) return false;
	int w = width > 0 ? width : (int)CGImageGetWidth(cg);
	int h = height > 0 ? height : (int)CGImageGetHeight(cg);
	img.width = w;
	img.height = h;
	img.bgra.assign((size_t)w * h * 4, 0);
	CGColorSpaceRef cs = CGColorSpaceCreateDeviceRGB();
	CGContextRef ctx = CGBitmapContextCreate(img.bgra.data(), w, h, 8, (size_t)w * 4, cs,
		kCGImageAlphaPremultipliedFirst | kCGBitmapByteOrder32Little);
	CGColorSpaceRelease(cs);
	if (!ctx) {
		CGImageRelease(cg);
		return false;
	}
	CGContextSetInterpolationQuality(ctx, kCGInterpolationHigh);
	CGContextDrawImage(ctx, CGRectMake(0, 0, w, h), cg);
	CGContextRelease(ctx);
	CGImageRelease(cg);
	return true;
}

}

namespace oplat {

bool LoadImageFile(const std::string &path, Image &img, int width, int height)
{
	@autoreleasepool {
		std::string p = path;
		for (char &c : p) if (c == '\\') c = '/';
		NSURL *url = [NSURL fileURLWithPath:[NSString stringWithUTF8String:p.c_str()]];
		return DecodeSource(CGImageSourceCreateWithURL((__bridge CFURLRef)url, nullptr), img, width, height);
	}
}

bool LoadImageMemory(const void *data, size_t size, Image &img, int width, int height)
{
	@autoreleasepool {
		NSData *d = [NSData dataWithBytesNoCopy:(void *)data length:size freeWhenDone:NO];
		return DecodeSource(CGImageSourceCreateWithData((__bridge CFDataRef)d, nullptr), img, width, height);
	}
}

bool SaveImageFile(const std::string &path, const uint8_t *pixels, int width, int height, int stride, int bpp, ImageFileType type, float quality)
{
	@autoreleasepool {
		if (bpp != 24 && bpp != 32) return false;
		std::vector<uint8_t> rgba((size_t)width * height * 4);
		for (int y = 0; y < height; y++) {
			const uint8_t *src = pixels + (size_t)y * stride;
			uint8_t *dst = rgba.data() + (size_t)y * width * 4;
			for (int x = 0; x < width; x++) {
				const uint8_t *s = src + x * (bpp / 8);
				dst[x * 4 + 0] = s[2];
				dst[x * 4 + 1] = s[1];
				dst[x * 4 + 2] = s[0];
				dst[x * 4 + 3] = 255;
			}
		}
		CGColorSpaceRef cs = CGColorSpaceCreateDeviceRGB();
		CGContextRef ctx = CGBitmapContextCreate(rgba.data(), width, height, 8, (size_t)width * 4, cs,
			kCGImageAlphaNoneSkipLast | kCGBitmapByteOrder32Big);
		CGColorSpaceRelease(cs);
		if (!ctx) return false;
		CGImageRef cg = CGBitmapContextCreateImage(ctx);
		CGContextRelease(ctx);
		if (!cg) return false;

		UTType *ut = type == IMGFILE_BMP ? UTTypeBMP : type == IMGFILE_PNG ? UTTypePNG : type == IMGFILE_TIF ? UTTypeTIFF : UTTypeJPEG;
		std::string p = path;
		for (char &c : p) if (c == '\\') c = '/';
		NSString *ns = [NSString stringWithUTF8String:p.c_str()];
		[[NSFileManager defaultManager] createDirectoryAtPath:[ns stringByDeletingLastPathComponent] withIntermediateDirectories:YES attributes:nil error:nil];
		NSURL *url = [NSURL fileURLWithPath:ns];
		CGImageDestinationRef dst = CGImageDestinationCreateWithURL((__bridge CFURLRef)url, (__bridge CFStringRef)ut.identifier, 1, nullptr);
		if (!dst) {
			CGImageRelease(cg);
			return false;
		}
		NSDictionary *props = @{ (__bridge NSString *)kCGImageDestinationLossyCompressionQuality: @(quality) };
		CGImageDestinationAddImage(dst, cg, (__bridge CFDictionaryRef)props);
		bool ok = CGImageDestinationFinalize(dst);
		CFRelease(dst);
		CGImageRelease(cg);
		return ok;
	}
}

bool CopyImageToClipboard(const uint8_t *pixels, int width, int height, int stride, int bpp)
{
	@autoreleasepool {
		if (bpp != 24 && bpp != 32) return false;
		NSBitmapImageRep *rep = [[NSBitmapImageRep alloc] initWithBitmapDataPlanes:nullptr pixelsWide:width pixelsHigh:height
			bitsPerSample:8 samplesPerPixel:3 hasAlpha:NO isPlanar:NO colorSpaceName:NSDeviceRGBColorSpace bytesPerRow:(NSInteger)width * 4 bitsPerPixel:32];
		if (!rep) return false;
		uint8_t *dst = rep.bitmapData;
		for (int y = 0; y < height; y++) {
			const uint8_t *src = pixels + (size_t)y * stride;
			uint8_t *row = dst + (size_t)y * width * 4;
			for (int x = 0; x < width; x++) {
				const uint8_t *s = src + x * (bpp / 8);
				row[x * 4 + 0] = s[2];
				row[x * 4 + 1] = s[1];
				row[x * 4 + 2] = s[0];
				row[x * 4 + 3] = 255;
			}
		}
		NSData *png = [rep representationUsingType:NSBitmapImageFileTypePNG properties:@{}];
		NSPasteboard *pb = [NSPasteboard generalPasteboard];
		[pb clearContents];
		return [pb setData:png forType:NSPasteboardTypePNG];
	}
}

}

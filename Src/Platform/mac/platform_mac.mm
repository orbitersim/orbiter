#import <AppKit/AppKit.h>
#import <QuartzCore/CAMetalLayer.h>
#import <Metal/Metal.h>
#import <Carbon/Carbon.h>
#import <UniformTypeIdentifiers/UniformTypeIdentifiers.h>
#include "OrbiterPlatform.h"
#include <deque>
#include <mutex>
#include <set>

@class OPView;

struct HWND__ {
	NSWindow *window = nil;
	OPView *view = nil;
	id delegate = nil;
	WNDPROC proc = nullptr;
	void *user = nullptr;
	bool alive = true;
	bool fullscreen = false;
};

namespace {

struct PostedMsg {
	HWND hWnd;
	UINT msg;
	WPARAM wParam;
	LPARAM lParam;
};

std::set<HWND> g_windows;
std::deque<PostedMsg> g_posted;
std::mutex g_postedLock;
bool g_quit = false;
int g_quitCode = 0;
BYTE g_keyState[256] = {};
std::vector<oplat::KeyEvent> g_keyEvents;
HWND g_capture = nullptr;
HWND g_keyCapture = nullptr;
int g_cursorHideCount = 0;
bool g_cursorHidden = false;
bool g_relativeMouse = false;
double g_virtualX = 0, g_virtualY = 0;

struct OPKeyMap {
	unsigned short mac;
	BYTE dik;
	BYTE vk;
};

const OPKeyMap kKeyMap[] = {
	{kVK_ANSI_A, 0x1E, 'A'}, {kVK_ANSI_S, 0x1F, 'S'}, {kVK_ANSI_D, 0x20, 'D'}, {kVK_ANSI_F, 0x21, 'F'},
	{kVK_ANSI_H, 0x23, 'H'}, {kVK_ANSI_G, 0x22, 'G'}, {kVK_ANSI_Z, 0x2C, 'Z'}, {kVK_ANSI_X, 0x2D, 'X'},
	{kVK_ANSI_C, 0x2E, 'C'}, {kVK_ANSI_V, 0x2F, 'V'}, {kVK_ISO_Section, 0x56, 0xE2}, {kVK_ANSI_B, 0x30, 'B'},
	{kVK_ANSI_Q, 0x10, 'Q'}, {kVK_ANSI_W, 0x11, 'W'}, {kVK_ANSI_E, 0x12, 'E'}, {kVK_ANSI_R, 0x13, 'R'},
	{kVK_ANSI_Y, 0x15, 'Y'}, {kVK_ANSI_T, 0x14, 'T'}, {kVK_ANSI_1, 0x02, '1'}, {kVK_ANSI_2, 0x03, '2'},
	{kVK_ANSI_3, 0x04, '3'}, {kVK_ANSI_4, 0x05, '4'}, {kVK_ANSI_6, 0x07, '6'}, {kVK_ANSI_5, 0x06, '5'},
	{kVK_ANSI_Equal, 0x0D, VK_OEM_PLUS}, {kVK_ANSI_9, 0x0A, '9'}, {kVK_ANSI_7, 0x08, '7'},
	{kVK_ANSI_Minus, 0x0C, VK_OEM_MINUS}, {kVK_ANSI_8, 0x09, '8'}, {kVK_ANSI_0, 0x0B, '0'},
	{kVK_ANSI_RightBracket, 0x1B, VK_OEM_6}, {kVK_ANSI_O, 0x18, 'O'}, {kVK_ANSI_U, 0x16, 'U'},
	{kVK_ANSI_LeftBracket, 0x1A, VK_OEM_4}, {kVK_ANSI_I, 0x17, 'I'}, {kVK_ANSI_P, 0x19, 'P'},
	{kVK_Return, 0x1C, VK_RETURN}, {kVK_ANSI_L, 0x26, 'L'}, {kVK_ANSI_J, 0x24, 'J'},
	{kVK_ANSI_Quote, 0x28, VK_OEM_7}, {kVK_ANSI_K, 0x25, 'K'}, {kVK_ANSI_Semicolon, 0x27, VK_OEM_1},
	{kVK_ANSI_Backslash, 0x2B, VK_OEM_5}, {kVK_ANSI_Comma, 0x33, VK_OEM_COMMA}, {kVK_ANSI_Slash, 0x35, VK_OEM_2},
	{kVK_ANSI_N, 0x31, 'N'}, {kVK_ANSI_M, 0x32, 'M'}, {kVK_ANSI_Period, 0x34, VK_OEM_PERIOD},
	{kVK_Tab, 0x0F, VK_TAB}, {kVK_Space, 0x39, VK_SPACE}, {kVK_ANSI_Grave, 0x29, VK_OEM_3},
	{kVK_Delete, 0x0E, VK_BACK}, {kVK_Escape, 0x01, VK_ESCAPE},
	{kVK_RightCommand, 0xDC, VK_RWIN}, {kVK_Command, 0xDB, VK_LWIN},
	{kVK_Shift, 0x2A, VK_LSHIFT}, {kVK_CapsLock, 0x3A, VK_CAPITAL}, {kVK_Option, 0x38, VK_LMENU},
	{kVK_Control, 0x1D, VK_LCONTROL}, {kVK_RightShift, 0x36, VK_RSHIFT}, {kVK_RightOption, 0xB8, VK_RMENU},
	{kVK_RightControl, 0x9D, VK_RCONTROL},
	{kVK_ANSI_KeypadDecimal, 0x53, VK_DECIMAL}, {kVK_ANSI_KeypadMultiply, 0x37, VK_MULTIPLY},
	{kVK_ANSI_KeypadPlus, 0x4E, VK_ADD}, {kVK_ANSI_KeypadClear, 0x45, VK_NUMLOCK},
	{kVK_ANSI_KeypadDivide, 0xB5, VK_DIVIDE}, {kVK_ANSI_KeypadEnter, 0x9C, VK_RETURN},
	{kVK_ANSI_KeypadMinus, 0x4A, VK_SUBTRACT}, {kVK_ANSI_KeypadEquals, 0x8D, VK_OEM_PLUS},
	{kVK_ANSI_Keypad0, 0x52, VK_NUMPAD0}, {kVK_ANSI_Keypad1, 0x4F, VK_NUMPAD1}, {kVK_ANSI_Keypad2, 0x50, VK_NUMPAD2},
	{kVK_ANSI_Keypad3, 0x51, VK_NUMPAD3}, {kVK_ANSI_Keypad4, 0x4B, VK_NUMPAD4}, {kVK_ANSI_Keypad5, 0x4C, VK_NUMPAD5},
	{kVK_ANSI_Keypad6, 0x4D, VK_NUMPAD6}, {kVK_ANSI_Keypad7, 0x47, VK_NUMPAD7}, {kVK_ANSI_Keypad8, 0x48, VK_NUMPAD8},
	{kVK_ANSI_Keypad9, 0x49, VK_NUMPAD9},
	{kVK_F1, 0x3B, VK_F1}, {kVK_F2, 0x3C, VK_F2}, {kVK_F3, 0x3D, VK_F3}, {kVK_F4, 0x3E, VK_F4},
	{kVK_F5, 0x3F, VK_F5}, {kVK_F6, 0x40, VK_F6}, {kVK_F7, 0x41, VK_F7}, {kVK_F8, 0x42, VK_F8},
	{kVK_F9, 0x43, VK_F9}, {kVK_F10, 0x44, VK_F10}, {kVK_F11, 0x57, VK_F11}, {kVK_F12, 0x58, VK_F12},
	{kVK_F13, 0xB7, VK_SNAPSHOT}, {kVK_F14, 0x46, VK_SCROLL}, {kVK_F15, 0xC5, VK_PAUSE},
	{kVK_Help, 0xD2, VK_INSERT}, {kVK_Home, 0xC7, VK_HOME}, {kVK_PageUp, 0xC9, VK_PRIOR},
	{kVK_ForwardDelete, 0xD3, VK_DELETE}, {kVK_End, 0xCF, VK_END}, {kVK_PageDown, 0xD1, VK_NEXT},
	{kVK_LeftArrow, 0xCB, VK_LEFT}, {kVK_RightArrow, 0xCD, VK_RIGHT}, {kVK_DownArrow, 0xD0, VK_DOWN},
	{kVK_UpArrow, 0xC8, VK_UP},
};

const OPKeyMap *FindKey(unsigned short mac)
{
	for (const auto &k : kKeyMap)
		if (k.mac == mac) return &k;
	return nullptr;
}

LRESULT Dispatch(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
	if (!hWnd || !hWnd->alive) return 0;
	if (hWnd->proc) return hWnd->proc(hWnd, msg, wParam, lParam);
	return DefWindowProcA(hWnd, msg, wParam, lParam);
}

float Scale(HWND hWnd)
{
	return hWnd && hWnd->window ? (float)hWnd->window.backingScaleFactor : 1.0f;
}

double MainScreenHeight()
{
	NSScreen *s = [NSScreen screens].firstObject;
	return s ? s.frame.size.height : 0;
}

void SetKey(const OPKeyMap *k, bool down)
{
	if (!k) return;
	BYTE prev = g_keyState[k->dik];
	g_keyState[k->dik] = down ? 0x80 : 0;
	if ((prev != 0) != down)
		g_keyEvents.push_back({k->dik, down});
}

WPARAM MouseKeyFlags(NSEvent *e)
{
	WPARAM f = 0;
	NSUInteger b = [NSEvent pressedMouseButtons];
	if (b & 1) f |= MK_LBUTTON;
	if (b & 2) f |= MK_RBUTTON;
	if (b & 4) f |= MK_MBUTTON;
	NSEventModifierFlags m = e ? e.modifierFlags : [NSEvent modifierFlags];
	if (m & NSEventModifierFlagShift) f |= MK_SHIFT;
	if (m & NSEventModifierFlagControl) f |= MK_CONTROL;
	return f;
}

void ApplyCursorVisibility()
{
	bool hide = g_cursorHideCount < 0 || g_relativeMouse;
	if (hide != g_cursorHidden) {
		if (hide) [NSCursor hide];
		else [NSCursor unhide];
		g_cursorHidden = hide;
	}
}

}

@interface OPWindow : NSWindow
@property (nonatomic, assign) BOOL keyable;
@end

@implementation OPWindow
- (BOOL)canBecomeKeyWindow { return self.keyable || [super canBecomeKeyWindow]; }
@end

@interface OPView : NSView <NSTextInputClient>
@property (nonatomic, assign) HWND owner;
@property (nonatomic, assign) BOOL metal;
@end

@implementation OPView {
	NSTrackingArea *_tracking;
	BOOL _inKeyDown;
}

- (BOOL)acceptsFirstResponder { return YES; }
- (BOOL)acceptsFirstMouse:(NSEvent *)e { return YES; }
- (BOOL)isFlipped { return YES; }
- (BOOL)wantsUpdateLayer { return YES; }

- (CALayer *)makeBackingLayer
{
	if (!self.metal) return [super makeBackingLayer];
	CAMetalLayer *layer = [CAMetalLayer layer];
	layer.device = MTLCreateSystemDefaultDevice();
	layer.pixelFormat = MTLPixelFormatBGRA8Unorm;
	layer.framebufferOnly = NO;
	layer.contentsScale = self.window ? self.window.backingScaleFactor : NSScreen.mainScreen.backingScaleFactor;
	return layer;
}

- (void)viewDidChangeBackingProperties
{
	[super viewDidChangeBackingProperties];
	self.layer.contentsScale = self.window.backingScaleFactor;
	[self updateDrawableSize];
}

- (void)updateDrawableSize
{
	if (![self.layer isKindOfClass:[CAMetalLayer class]]) return;
	CGFloat s = self.window ? self.window.backingScaleFactor : 1.0;
	NSSize sz = self.bounds.size;
	((CAMetalLayer *)self.layer).drawableSize = CGSizeMake(MAX(1.0, sz.width * s), MAX(1.0, sz.height * s));
}

- (void)setFrameSize:(NSSize)size
{
	[super setFrameSize:size];
	[self updateDrawableSize];
	float s = Scale(self.owner);
	Dispatch(self.owner, WM_SIZE, SIZE_RESTORED, MAKELPARAM((int)(size.width * s), (int)(size.height * s)));
}

- (void)updateTrackingAreas
{
	if (_tracking) [self removeTrackingArea:_tracking];
	_tracking = [[NSTrackingArea alloc] initWithRect:self.bounds
		options:NSTrackingMouseMoved | NSTrackingActiveAlways | NSTrackingInVisibleRect | NSTrackingMouseEnteredAndExited
		owner:self userInfo:nil];
	[self addTrackingArea:_tracking];
	[super updateTrackingAreas];
}

- (LPARAM)mouseLParam:(NSEvent *)e
{
	NSPoint p = [self convertPoint:e.locationInWindow fromView:nil];
	float s = Scale(self.owner);
	return MAKELPARAM((short)(p.x * s), (short)(p.y * s));
}

- (void)mouseMsg:(NSEvent *)e msg:(UINT)msg
{
	if (g_relativeMouse && (e.type == NSEventTypeMouseMoved || e.type == NSEventTypeLeftMouseDragged ||
		e.type == NSEventTypeRightMouseDragged || e.type == NSEventTypeOtherMouseDragged)) {
		CGFloat sc = NSScreen.mainScreen.backingScaleFactor;
		g_virtualX += e.deltaX * sc;
		g_virtualY += e.deltaY * sc;
	}
	Dispatch(self.owner, msg, MouseKeyFlags(e), [self mouseLParam:e]);
}

- (void)mouseDown:(NSEvent *)e
{
	[self mouseMsg:e msg:e.clickCount == 2 ? WM_LBUTTONDBLCLK : WM_LBUTTONDOWN];
}
- (void)mouseUp:(NSEvent *)e { [self mouseMsg:e msg:WM_LBUTTONUP]; }
- (void)rightMouseDown:(NSEvent *)e { [self mouseMsg:e msg:e.clickCount == 2 ? WM_RBUTTONDBLCLK : WM_RBUTTONDOWN]; }
- (void)rightMouseUp:(NSEvent *)e { [self mouseMsg:e msg:WM_RBUTTONUP]; }
- (void)otherMouseDown:(NSEvent *)e { if (e.buttonNumber == 2) [self mouseMsg:e msg:WM_MBUTTONDOWN]; }
- (void)otherMouseUp:(NSEvent *)e { if (e.buttonNumber == 2) [self mouseMsg:e msg:WM_MBUTTONUP]; }
- (void)mouseMoved:(NSEvent *)e { [self mouseMsg:e msg:WM_MOUSEMOVE]; }
- (void)mouseDragged:(NSEvent *)e { [self mouseMsg:e msg:WM_MOUSEMOVE]; }
- (void)rightMouseDragged:(NSEvent *)e { [self mouseMsg:e msg:WM_MOUSEMOVE]; }
- (void)otherMouseDragged:(NSEvent *)e { [self mouseMsg:e msg:WM_MOUSEMOVE]; }
- (void)mouseExited:(NSEvent *)e { Dispatch(self.owner, WM_MOUSELEAVE, 0, 0); }

- (void)scrollWheel:(NSEvent *)e
{
	double dy = e.scrollingDeltaY, dx = e.scrollingDeltaX;
	if (e.hasPreciseScrollingDeltas) { dy *= 0.1; dx *= 0.1; }
	LPARAM lp = [self mouseLParam:e];
	if (dy != 0) {
		short d = (short)MAX(-32000.0, MIN(32000.0, dy * WHEEL_DELTA));
		Dispatch(self.owner, WM_MOUSEWHEEL, MAKEWPARAM(MouseKeyFlags(e), d), lp);
	}
	if (dx != 0) {
		short d = (short)MAX(-32000.0, MIN(32000.0, -dx * WHEEL_DELTA));
		Dispatch(self.owner, WM_MOUSEHWHEEL, MAKEWPARAM(MouseKeyFlags(e), d), lp);
	}
}

- (LPARAM)keyLParam:(const OPKeyMap *)k up:(BOOL)up repeat:(BOOL)rep
{
	DWORD dik = k ? k->dik : 0;
	DWORD lp = 1 | ((dik & 0x7F) << 16);
	if (dik & 0x80) lp |= 1u << 24;
	if (rep) lp |= 1u << 30;
	if (up) lp |= (1u << 30) | (1u << 31);
	return (LPARAM)lp;
}

- (void)keyDown:(NSEvent *)e
{
	const OPKeyMap *k = FindKey(e.keyCode);
	if (!e.isARepeat) SetKey(k, true);
	if (k) {
		BOOL alt = (e.modifierFlags & NSEventModifierFlagOption) != 0;
		Dispatch(self.owner, alt ? WM_SYSKEYDOWN : WM_KEYDOWN, k->vk, [self keyLParam:k up:NO repeat:e.isARepeat]);
	}
	if (!(e.modifierFlags & (NSEventModifierFlagCommand | NSEventModifierFlagControl))) {
		_inKeyDown = YES;
		[self interpretKeyEvents:@[e]];
		_inKeyDown = NO;
	}
}

- (void)keyUp:(NSEvent *)e
{
	const OPKeyMap *k = FindKey(e.keyCode);
	SetKey(k, false);
	if (k) {
		BOOL alt = (e.modifierFlags & NSEventModifierFlagOption) != 0;
		Dispatch(self.owner, alt ? WM_SYSKEYUP : WM_KEYUP, k->vk, [self keyLParam:k up:YES repeat:NO]);
	}
}

- (void)flagsChanged:(NSEvent *)e
{
	const OPKeyMap *k = FindKey(e.keyCode);
	if (!k) return;
	NSEventModifierFlags f = e.modifierFlags;
	bool down;
	switch (e.keyCode) {
	case kVK_Shift: down = (f & NX_DEVICELSHIFTKEYMASK) != 0; break;
	case kVK_RightShift: down = (f & NX_DEVICERSHIFTKEYMASK) != 0; break;
	case kVK_Control: down = (f & NX_DEVICELCTLKEYMASK) != 0; break;
	case kVK_RightControl: down = (f & NX_DEVICERCTLKEYMASK) != 0; break;
	case kVK_Option: down = (f & NX_DEVICELALTKEYMASK) != 0; break;
	case kVK_RightOption: down = (f & NX_DEVICERALTKEYMASK) != 0; break;
	case kVK_Command: down = (f & NX_DEVICELCMDKEYMASK) != 0; break;
	case kVK_RightCommand: down = (f & NX_DEVICERCMDKEYMASK) != 0; break;
	case kVK_CapsLock: down = (f & NSEventModifierFlagCapsLock) != 0; break;
	default: return;
	}
	SetKey(k, down);
	UINT generic = (k->vk == VK_LSHIFT || k->vk == VK_RSHIFT) ? VK_SHIFT :
		(k->vk == VK_LCONTROL || k->vk == VK_RCONTROL) ? VK_CONTROL :
		(k->vk == VK_LMENU || k->vk == VK_RMENU) ? VK_MENU : k->vk;
	UINT msg = down ? ((generic == VK_MENU) ? WM_SYSKEYDOWN : WM_KEYDOWN) : ((generic == VK_MENU) ? WM_SYSKEYUP : WM_KEYUP);
	Dispatch(self.owner, msg, generic, [self keyLParam:k up:!down repeat:NO]);
}

- (void)insertText:(id)string replacementRange:(NSRange)range
{
	NSString *s = [string isKindOfClass:[NSAttributedString class]] ? [string string] : string;
	for (NSUInteger i = 0; i < s.length; i++) {
		unichar c = [s characterAtIndex:i];
		if (c >= 0xF700 && c <= 0xF8FF) continue;
		Dispatch(self.owner, WM_CHAR, c, 1);
	}
}

- (void)doCommandBySelector:(SEL)sel {}
- (void)setMarkedText:(id)s selectedRange:(NSRange)a replacementRange:(NSRange)b {}
- (void)unmarkText {}
- (NSRange)selectedRange { return NSMakeRange(NSNotFound, 0); }
- (NSRange)markedRange { return NSMakeRange(NSNotFound, 0); }
- (BOOL)hasMarkedText { return NO; }
- (NSAttributedString *)attributedSubstringForProposedRange:(NSRange)r actualRange:(NSRangePointer)a { return nil; }
- (NSArray<NSAttributedStringKey> *)validAttributesForMarkedText { return @[]; }
- (NSRect)firstRectForCharacterRange:(NSRange)r actualRange:(NSRangePointer)a { return NSZeroRect; }
- (NSUInteger)characterIndexForPoint:(NSPoint)p { return NSNotFound; }

@end

@interface OPWindowDelegate : NSObject <NSWindowDelegate>
@property (nonatomic, assign) HWND owner;
@end

@implementation OPWindowDelegate

- (BOOL)windowShouldClose:(NSWindow *)w
{
	Dispatch(self.owner, WM_CLOSE, 0, 0);
	return NO;
}

- (void)windowDidBecomeKey:(NSNotification *)n
{
	Dispatch(self.owner, WM_ACTIVATE, WA_ACTIVE, 0);
	Dispatch(self.owner, WM_SETFOCUS, 0, 0);
}

- (void)windowDidResignKey:(NSNotification *)n
{
	oplat::ClearKeyState();
	Dispatch(self.owner, WM_KILLFOCUS, 0, 0);
	Dispatch(self.owner, WM_ACTIVATE, WA_INACTIVE, 0);
}

- (void)windowDidMove:(NSNotification *)n
{
	NSRect f = self.owner->window.frame;
	Dispatch(self.owner, WM_MOVE, 0, MAKELPARAM((int)f.origin.x, (int)(MainScreenHeight() - f.origin.y - f.size.height)));
}

- (void)windowWillStartLiveResize:(NSNotification *)n { Dispatch(self.owner, WM_ENTERSIZEMOVE, 0, 0); }
- (void)windowDidEndLiveResize:(NSNotification *)n { Dispatch(self.owner, WM_EXITSIZEMOVE, 0, 0); }
- (void)windowDidEnterFullScreen:(NSNotification *)n { self.owner->fullscreen = true; }
- (void)windowDidExitFullScreen:(NSNotification *)n { self.owner->fullscreen = false; }

@end

@interface OPAppDelegate : NSObject <NSApplicationDelegate>
@end

@implementation OPAppDelegate

- (NSApplicationTerminateReply)applicationShouldTerminate:(NSApplication *)app
{
	bool handled = false;
	for (HWND h : g_windows) {
		if (h->alive && h->window.isVisible) {
			Dispatch(h, WM_CLOSE, 0, 0);
			handled = true;
			break;
		}
	}
	if (!handled) PostQuitMessage(0);
	return NSTerminateCancel;
}

- (BOOL)applicationShouldTerminateAfterLastWindowClosed:(NSApplication *)app { return NO; }

@end

namespace {

void BuildMenu()
{
	NSMenu *bar = [[NSMenu alloc] init];
	NSMenuItem *appItem = [[NSMenuItem alloc] init];
	[bar addItem:appItem];
	NSMenu *appMenu = [[NSMenu alloc] init];
	NSString *name = [[NSProcessInfo processInfo] processName];
	[appMenu addItemWithTitle:[@"Hide " stringByAppendingString:name] action:@selector(hide:) keyEquivalent:@"h"];
	[appMenu addItem:[NSMenuItem separatorItem]];
	[appMenu addItemWithTitle:[@"Quit " stringByAppendingString:name] action:@selector(terminate:) keyEquivalent:@"q"];
	appItem.submenu = appMenu;

	NSMenuItem *winItem = [[NSMenuItem alloc] init];
	[bar addItem:winItem];
	NSMenu *winMenu = [[NSMenu alloc] initWithTitle:@"Window"];
	[winMenu addItemWithTitle:@"Minimize" action:@selector(performMiniaturize:) keyEquivalent:@"m"];
	NSMenuItem *fs = [winMenu addItemWithTitle:@"Enter Full Screen" action:@selector(toggleFullScreen:) keyEquivalent:@"f"];
	fs.keyEquivalentModifierMask = NSEventModifierFlagCommand | NSEventModifierFlagControl;
	winItem.submenu = winMenu;
	NSApp.windowsMenu = winMenu;
	NSApp.mainMenu = bar;
}

OPAppDelegate *g_appDelegate = nil;

}

namespace oplat {

void Init()
{
	static bool done = false;
	if (done) return;
	done = true;
	@autoreleasepool {
		[NSApplication sharedApplication];
		[NSApp setActivationPolicy:NSApplicationActivationPolicyRegular];
		g_appDelegate = [[OPAppDelegate alloc] init];
		NSApp.delegate = g_appDelegate;
		BuildMenu();
		[NSApp finishLaunching];
		[NSApp activateIgnoringOtherApps:YES];
		CGEventSourceRef src = CGEventSourceCreate(kCGEventSourceStateCombinedSessionState);
		if (src) {
			CGEventSourceSetLocalEventsSuppressionInterval(src, 0.0);
			CFRelease(src);
		}
	}
}

HWND OpenWindow(const WindowDesc &d)
{
	Init();
	HWND h = new HWND__;
	h->proc = d.proc;
	h->user = d.userData;
	@autoreleasepool {
		NSWindowStyleMask style = d.borderless ? NSWindowStyleMaskBorderless :
			(NSWindowStyleMaskTitled | NSWindowStyleMaskClosable | NSWindowStyleMaskMiniaturizable | (d.resizable ? NSWindowStyleMaskResizable : 0));
		NSScreen *screen = NSScreen.mainScreen;
		NSRect vis = screen.visibleFrame;
		CGFloat s = screen.backingScaleFactor;
		NSRect content = NSMakeRect(0, 0, d.width / s, d.height / s);
		OPWindow *w = [[OPWindow alloc] initWithContentRect:content styleMask:style backing:NSBackingStoreBuffered defer:NO];
		w.keyable = d.borderless;
		w.releasedWhenClosed = NO;
		w.title = [NSString stringWithUTF8String:d.title.c_str()];
		w.acceptsMouseMovedEvents = YES;
		w.collectionBehavior = NSWindowCollectionBehaviorFullScreenPrimary;
		w.tabbingMode = NSWindowTabbingModeDisallowed;
		OPView *v = [[OPView alloc] initWithFrame:content];
		v.owner = h;
		v.metal = d.metal;
		v.wantsLayer = YES;
		w.contentView = v;
		[w makeFirstResponder:v];
		OPWindowDelegate *del = [[OPWindowDelegate alloc] init];
		del.owner = h;
		w.delegate = del;
		h->window = w;
		h->view = v;
		h->delegate = del;
		if (d.x >= 0 && d.y >= 0) {
			NSRect f = [w frameRectForContentRect:content];
			[w setFrameTopLeftPoint:NSMakePoint(d.x / s, MainScreenHeight() - d.y / s)];
			(void)f;
		} else {
			NSRect f = w.frame;
			[w setFrameOrigin:NSMakePoint(NSMidX(vis) - f.size.width / 2, NSMidY(vis) - f.size.height / 2)];
		}
		[v updateDrawableSize];
		g_windows.insert(h);
		if (d.visible) [w makeKeyAndOrderFront:nil];
		if (d.borderless) w.collectionBehavior = NSWindowCollectionBehaviorFullScreenAuxiliary;
		if (d.fullscreen) SetFullscreen(h, true);
	}
	Dispatch(h, WM_CREATE, 0, 0);
	return h;
}

HWND OpenBackdropWindow()
{
	Init();
	HWND h = new HWND__;
	@autoreleasepool {
		NSScreen *screen = NSScreen.mainScreen;
		NSWindow *w = [[NSWindow alloc] initWithContentRect:screen.frame styleMask:NSWindowStyleMaskBorderless backing:NSBackingStoreBuffered defer:NO];
		w.releasedWhenClosed = NO;
		w.backgroundColor = NSColor.blackColor;
		w.ignoresMouseEvents = YES;
		w.collectionBehavior = NSWindowCollectionBehaviorStationary | NSWindowCollectionBehaviorIgnoresCycle;
		[w setFrame:screen.frame display:NO];
		[w orderFront:nil];
		h->window = w;
		g_windows.insert(h);
	}
	return h;
}

void SetWindowProc(HWND hWnd, WNDPROC proc) { if (hWnd) hWnd->proc = proc; }
WNDPROC GetWindowProc(HWND hWnd) { return hWnd ? hWnd->proc : nullptr; }
void SetWindowUserData(HWND hWnd, void *data) { if (hWnd) hWnd->user = data; }
void *GetWindowUserData(HWND hWnd) { return hWnd ? hWnd->user : nullptr; }

void *GetMetalLayer(HWND hWnd)
{
	if (!hWnd || !hWnd->view) return nullptr;
	CALayer *l = hWnd->view.layer;
	return [l isKindOfClass:[CAMetalLayer class]] ? (__bridge void *)l : nullptr;
}

float GetBackingScale(HWND hWnd) { return hWnd ? Scale(hWnd) : (float)NSScreen.mainScreen.backingScaleFactor; }

void GetDrawableSize(HWND hWnd, int *w, int *h)
{
	if (!hWnd || !hWnd->view) { *w = *h = 0; return; }
	float s = Scale(hWnd);
	NSSize sz = hWnd->view.bounds.size;
	*w = (int)(sz.width * s);
	*h = (int)(sz.height * s);
}

void SetWindowClientSize(HWND hWnd, int w, int h)
{
	if (!hWnd || !hWnd->window) return;
	float s = Scale(hWnd);
	NSRect top = hWnd->window.frame;
	[hWnd->window setContentSize:NSMakeSize(w / s, h / s)];
	NSRect nf = hWnd->window.frame;
	[hWnd->window setFrameTopLeftPoint:NSMakePoint(nf.origin.x, top.origin.y + top.size.height)];
}

void SetWindowPosition(HWND hWnd, int x, int y)
{
	if (!hWnd || !hWnd->window) return;
	float s = Scale(hWnd);
	[hWnd->window setFrameTopLeftPoint:NSMakePoint(x / s, MainScreenHeight() - y / s)];
}

void *NativeWindow(HWND hWnd)
{
	return hWnd && hWnd->window ? (__bridge void *)hWnd->window : nullptr;
}

static float DesktopScale()
{
	NSScreen *s = [NSScreen screens].firstObject;
	return s ? (float)s.backingScaleFactor : 1.0f;
}

void GetClientScreenRect(HWND hWnd, int *x, int *y, int *w, int *h)
{
	*x = *y = *w = *h = 0;
	if (!hWnd || !hWnd->window) return;
	float s = DesktopScale();
	NSRect c = [hWnd->window contentRectForFrameRect:hWnd->window.frame];
	*x = (int)lround(c.origin.x * s);
	*y = (int)lround((MainScreenHeight() - c.origin.y - c.size.height) * s);
	*w = (int)lround(c.size.width * s);
	*h = (int)lround(c.size.height * s);
}

void SetClientScreenRect(HWND hWnd, int x, int y, int w, int h)
{
	if (!hWnd || !hWnd->window) return;
	float s = DesktopScale();
	NSRect c = NSMakeRect(x / s, MainScreenHeight() - (y + h) / s, w / s, h / s);
	[hWnd->window setFrame:[hWnd->window frameRectForContentRect:c] display:YES];
}

void ShowWindowNoActivate(HWND hWnd)
{
	if (hWnd && hWnd->window) [hWnd->window orderFront:nil];
}

void FocusWindow(HWND hWnd)
{
	if (hWnd && hWnd->window) [hWnd->window makeKeyAndOrderFront:nil];
}

bool IsWindowFocused(HWND hWnd)
{
	return hWnd && hWnd->window && hWnd->window.isKeyWindow;
}

bool IsWindowMinimized(HWND hWnd)
{
	return hWnd && hWnd->window && hWnd->window.miniaturized;
}

void SetWindowAlpha(HWND hWnd, float alpha)
{
	if (hWnd && hWnd->window) hWnd->window.alphaValue = alpha;
}

void AttachChildWindow(HWND parent, HWND child)
{
	if (!parent || !parent->window || !child || !child->window) return;
	[parent->window addChildWindow:child->window ordered:NSWindowAbove];
}

std::vector<Monitor> Monitors()
{
	std::vector<Monitor> out;
	float s = DesktopScale();
	double top = MainScreenHeight();
	for (NSScreen *scr in [NSScreen screens]) {
		NSRect f = scr.frame, v = scr.visibleFrame;
		Monitor m;
		m.x = (int)lround(f.origin.x * s);
		m.y = (int)lround((top - f.origin.y - f.size.height) * s);
		m.w = (int)lround(f.size.width * s);
		m.h = (int)lround(f.size.height * s);
		m.workX = (int)lround(v.origin.x * s);
		m.workY = (int)lround((top - v.origin.y - v.size.height) * s);
		m.workW = (int)lround(v.size.width * s);
		m.workH = (int)lround(v.size.height * s);
		m.scale = (float)scr.backingScaleFactor / s;
		out.push_back(m);
	}
	return out;
}

void CursorScreenPos(int *x, int *y)
{
	float s = DesktopScale();
	NSPoint p = [NSEvent mouseLocation];
	*x = (int)lround(p.x * s);
	*y = (int)lround((MainScreenHeight() - p.y) * s);
}

bool IsFullscreen(HWND hWnd) { return hWnd && hWnd->fullscreen; }

void SetFullscreen(HWND hWnd, bool fullscreen)
{
	if (!hWnd || !hWnd->window) return;
	bool cur = (hWnd->window.styleMask & NSWindowStyleMaskFullScreen) != 0;
	if (cur != fullscreen) [hWnd->window toggleFullScreen:nil];
	hWnd->fullscreen = fullscreen;
}

void SetMinClientSize(HWND hWnd, int w, int h)
{
	if (!hWnd || !hWnd->window) return;
	float s = Scale(hWnd);
	hWnd->window.contentMinSize = NSMakeSize(w / s, h / s);
}

bool PumpEvents(bool wait)
{
	@autoreleasepool {
		for (;;) {
			std::deque<PostedMsg> posted;
			{
				std::lock_guard<std::mutex> lk(g_postedLock);
				posted.swap(g_posted);
			}
			for (auto &m : posted) {
				if (m.msg == WM_QUIT) {
					g_quit = true;
					g_quitCode = (int)m.wParam;
				} else if (m.hWnd) {
					if (g_windows.count(m.hWnd)) Dispatch(m.hWnd, m.msg, m.wParam, m.lParam);
				}
			}
			if (g_quit) return false;
			NSEvent *e = [NSApp nextEventMatchingMask:NSEventMaskAny
				untilDate:(wait && posted.empty()) ? [NSDate distantFuture] : [NSDate distantPast]
				inMode:NSDefaultRunLoopMode dequeue:YES];
			if (!e) break;
			[NSApp sendEvent:e];
			wait = false;
		}
	}
	return !g_quit;
}

bool QuitRequested() { return g_quit; }
int QuitCode() { return g_quitCode; }

void WakeEventLoop()
{
	@autoreleasepool {
		NSEvent *e = [NSEvent otherEventWithType:NSEventTypeApplicationDefined location:NSZeroPoint modifierFlags:0
			timestamp:0 windowNumber:0 context:nil subtype:0 data1:0 data2:0];
		[NSApp postEvent:e atStart:NO];
	}
}

const BYTE *KeyState() { return g_keyState; }

void TakeKeyEvents(std::vector<KeyEvent> &events)
{
	events.swap(g_keyEvents);
	g_keyEvents.clear();
}

void ClearKeyState()
{
	for (int i = 0; i < 256; i++)
		if (g_keyState[i]) {
			g_keyState[i] = 0;
			g_keyEvents.push_back({(DWORD)i, false});
		}
}

void SetKeyCaptureWindow(HWND hWnd) { g_keyCapture = hWnd; }

void SetCursorHidden(bool hidden)
{
	g_cursorHideCount = hidden ? -1 : 0;
	ApplyCursorVisibility();
}

void SetRelativeMouse(bool enabled)
{
	if (g_relativeMouse == enabled) return;
	if (enabled) {
		NSPoint p = [NSEvent mouseLocation];
		CGFloat sc = NSScreen.mainScreen.backingScaleFactor;
		g_virtualX = p.x * sc;
		g_virtualY = (MainScreenHeight() - p.y) * sc;
	}
	g_relativeMouse = enabled;
	CGAssociateMouseAndMouseCursorPosition(enabled ? false : true);
	ApplyCursorVisibility();
}

void SetCursorShape(int shape)
{
	NSCursor *c;
	switch (shape) {
	case CURSOR_CROSS: c = NSCursor.crosshairCursor; break;
	case CURSOR_HAND: c = NSCursor.pointingHandCursor; break;
	case CURSOR_IBEAM: c = NSCursor.IBeamCursor; break;
	case CURSOR_RESIZE_EW: c = NSCursor.resizeLeftRightCursor; break;
	case CURSOR_RESIZE_NS: c = NSCursor.resizeUpDownCursor; break;
	default: c = NSCursor.arrowCursor; break;
	}
	[c set];
}

std::vector<DisplayMode> DisplayModes()
{
	std::vector<DisplayMode> out;
	CFArrayRef modes = CGDisplayCopyAllDisplayModes(CGMainDisplayID(), nullptr);
	if (!modes) return out;
	for (CFIndex i = 0; i < CFArrayGetCount(modes); i++) {
		CGDisplayModeRef m = (CGDisplayModeRef)CFArrayGetValueAtIndex(modes, i);
		DisplayMode dm{(int)CGDisplayModeGetPixelWidth(m), (int)CGDisplayModeGetPixelHeight(m), CGDisplayModeGetRefreshRate(m)};
		bool dup = false;
		for (auto &o : out)
			if (o.width == dm.width && o.height == dm.height && o.refresh == dm.refresh) dup = true;
		if (!dup) out.push_back(dm);
	}
	CFRelease(modes);
	return out;
}

DisplayMode CurrentDisplayMode()
{
	CGDisplayModeRef m = CGDisplayCopyDisplayMode(CGMainDisplayID());
	DisplayMode dm{(int)CGDisplayModeGetPixelWidth(m), (int)CGDisplayModeGetPixelHeight(m), CGDisplayModeGetRefreshRate(m)};
	CGDisplayModeRelease(m);
	return dm;
}

std::string AppSupportDir()
{
	@autoreleasepool {
		NSArray *dirs = NSSearchPathForDirectoriesInDomains(NSApplicationSupportDirectory, NSUserDomainMask, YES);
		NSString *d = [dirs.firstObject stringByAppendingPathComponent:@"Orbiter"];
		[[NSFileManager defaultManager] createDirectoryAtPath:d withIntermediateDirectories:YES attributes:nil error:nil];
		return d.UTF8String;
	}
}

std::string ResourceDir()
{
	@autoreleasepool {
		return NSBundle.mainBundle.resourcePath.UTF8String;
	}
}

bool OpenURL(const std::string &url)
{
	@autoreleasepool {
		NSString *s = [NSString stringWithUTF8String:url.c_str()];
		NSURL *u = [s containsString:@"://"] ? [NSURL URLWithString:s] : [NSURL fileURLWithPath:s];
		return u && [[NSWorkspace sharedWorkspace] openURL:u];
	}
}

bool RevealInFinder(const std::string &path)
{
	@autoreleasepool {
		NSURL *u = [NSURL fileURLWithPath:[NSString stringWithUTF8String:path.c_str()]];
		[[NSWorkspace sharedWorkspace] activateFileViewerSelectingURLs:@[u]];
		return true;
	}
}

static NSArray<UTType *> *ContentTypes(const std::vector<std::string> &ext)
{
	NSMutableArray *a = [NSMutableArray array];
	for (auto &e : ext) {
		UTType *t = [UTType typeWithFilenameExtension:[NSString stringWithUTF8String:e.c_str()]];
		if (t) [a addObject:t];
	}
	return a;
}

std::string OpenFileDialog(const std::string &title, const std::string &dir, const std::vector<std::string> &extensions)
{
	@autoreleasepool {
		NSOpenPanel *p = [NSOpenPanel openPanel];
		p.title = [NSString stringWithUTF8String:title.c_str()];
		if (!dir.empty()) p.directoryURL = [NSURL fileURLWithPath:[NSString stringWithUTF8String:dir.c_str()]];
		if (!extensions.empty()) p.allowedContentTypes = ContentTypes(extensions);
		if ([p runModal] != NSModalResponseOK) return "";
		return p.URL.path.UTF8String;
	}
}

std::string SaveFileDialog(const std::string &title, const std::string &dir, const std::string &name, const std::vector<std::string> &extensions)
{
	@autoreleasepool {
		NSSavePanel *p = [NSSavePanel savePanel];
		p.title = [NSString stringWithUTF8String:title.c_str()];
		if (!dir.empty()) p.directoryURL = [NSURL fileURLWithPath:[NSString stringWithUTF8String:dir.c_str()]];
		p.nameFieldStringValue = [NSString stringWithUTF8String:name.c_str()];
		if (!extensions.empty()) p.allowedContentTypes = ContentTypes(extensions);
		if ([p runModal] != NSModalResponseOK) return "";
		return p.URL.path.UTF8String;
	}
}

}

extern "C" {

int MessageBoxA(HWND hWnd, const char *text, const char *caption, UINT type)
{
	fprintf(stderr, "%s: %s\n", caption ? caption : "", text ? text : "");
	if (!NSApp) return IDOK;
	@autoreleasepool {
		NSAlert *a = [[NSAlert alloc] init];
		a.messageText = caption ? [NSString stringWithUTF8String:caption] : @"";
		a.informativeText = text ? [NSString stringWithUTF8String:text] : @"";
		UINT icon = type & 0xF0;
		a.alertStyle = icon == MB_ICONERROR ? NSAlertStyleCritical : icon == MB_ICONWARNING ? NSAlertStyleWarning : NSAlertStyleInformational;
		std::vector<int> ids;
		switch (type & 0x0F) {
		case MB_OKCANCEL: ids = {IDOK, IDCANCEL}; break;
		case MB_YESNO: ids = {IDYES, IDNO}; break;
		case MB_YESNOCANCEL: ids = {IDYES, IDNO, IDCANCEL}; break;
		case MB_RETRYCANCEL: ids = {IDRETRY, IDCANCEL}; break;
		case MB_ABORTRETRYIGNORE: ids = {IDABORT, IDRETRY, IDIGNORE}; break;
		default: ids = {IDOK}; break;
		}
		for (int id : ids) {
			NSString *t = id == IDOK ? @"OK" : id == IDCANCEL ? @"Cancel" : id == IDYES ? @"Yes" : id == IDNO ? @"No" :
				id == IDRETRY ? @"Retry" : id == IDABORT ? @"Abort" : @"Ignore";
			[a addButtonWithTitle:t];
		}
		bool hid = g_cursorHidden;
		if (hid) [NSCursor unhide];
		NSModalResponse r = [a runModal];
		if (hid) [NSCursor hide];
		size_t idx = (size_t)(r - NSAlertFirstButtonReturn);
		return idx < ids.size() ? ids[idx] : ids.back();
	}
}

WINBOOL MessageBeep(UINT) { NSBeep(); return TRUE; }

int GetSystemMetrics(int index)
{
	NSScreen *s = NSScreen.mainScreen;
	CGFloat sc = s.backingScaleFactor;
	switch (index) {
	case SM_CXSCREEN: case SM_CXFULLSCREEN: case SM_CXMAXIMIZED: return (int)(s.frame.size.width * sc);
	case SM_CYSCREEN: case SM_CYFULLSCREEN: case SM_CYMAXIMIZED: return (int)(s.frame.size.height * sc);
	case SM_CXVIRTUALSCREEN: case SM_CYVIRTUALSCREEN: {
		NSRect u = NSZeroRect;
		for (NSScreen *x in NSScreen.screens) u = NSUnionRect(u, x.frame);
		return (int)((index == SM_CXVIRTUALSCREEN ? u.size.width : u.size.height) * sc);
	}
	case SM_XVIRTUALSCREEN: case SM_YVIRTUALSCREEN: return 0;
	case SM_CMONITORS: return (int)NSScreen.screens.count;
	case SM_CYCAPTION: return (int)(28 * sc);
	case SM_CXBORDER: case SM_CYBORDER: return 1;
	case SM_CXFRAME: case SM_CYFRAME: case SM_CXDLGFRAME: case SM_CYDLGFRAME: return 0;
	case SM_CXVSCROLL: case SM_CYHSCROLL: return (int)(15 * sc);
	case SM_CYMENU: return 0;
	default: return 0;
	}
}

WINBOOL GetCursorPos(POINT *pt)
{
	if (g_relativeMouse) {
		pt->x = (LONG)lround(g_virtualX);
		pt->y = (LONG)lround(g_virtualY);
		return TRUE;
	}
	NSPoint p = [NSEvent mouseLocation];
	CGFloat sc = NSScreen.mainScreen.backingScaleFactor;
	pt->x = (LONG)(p.x * sc);
	pt->y = (LONG)((MainScreenHeight() - p.y) * sc);
	return TRUE;
}

WINBOOL SetCursorPos(int x, int y)
{
	if (g_relativeMouse) {
		g_virtualX = x;
		g_virtualY = y;
		return TRUE;
	}
	CGFloat sc = NSScreen.mainScreen.backingScaleFactor;
	CGWarpMouseCursorPosition(CGPointMake(x / sc, y / sc));
	return TRUE;
}

int ShowCursor(WINBOOL show)
{
	g_cursorHideCount += show ? 1 : -1;
	ApplyCursorVisibility();
	return g_cursorHideCount;
}

static int VkToDik(int vk)
{
	switch (vk) {
	case VK_SHIFT: return -1;
	case VK_CONTROL: return -2;
	case VK_MENU: return -3;
	}
	for (const auto &k : kKeyMap)
		if (k.vk == vk) return k.dik;
	return 0;
}

SHORT GetAsyncKeyState(int vkey)
{
	if (vkey == VK_LBUTTON) return ([NSEvent pressedMouseButtons] & 1) ? (SHORT)0x8000 : 0;
	if (vkey == VK_RBUTTON) return ([NSEvent pressedMouseButtons] & 2) ? (SHORT)0x8000 : 0;
	if (vkey == VK_MBUTTON) return ([NSEvent pressedMouseButtons] & 4) ? (SHORT)0x8000 : 0;
	int d = VkToDik(vkey);
	bool down;
	if (d == -1) down = g_keyState[0x2A] || g_keyState[0x36];
	else if (d == -2) down = g_keyState[0x1D] || g_keyState[0x9D];
	else if (d == -3) down = g_keyState[0x38] || g_keyState[0xB8];
	else down = d > 0 && g_keyState[d];
	return down ? (SHORT)0x8000 : 0;
}

SHORT GetKeyState(int vkey)
{
	if (vkey == VK_CAPITAL) return ([NSEvent modifierFlags] & NSEventModifierFlagCapsLock) ? 1 : 0;
	if (vkey == VK_NUMLOCK) return 1;
	return GetAsyncKeyState(vkey);
}

WINBOOL ScreenToClient(HWND hWnd, POINT *pt)
{
	if (!hWnd || !hWnd->view) return FALSE;
	CGFloat sc = NSScreen.mainScreen.backingScaleFactor;
	NSPoint scr = NSMakePoint(pt->x / sc, MainScreenHeight() - pt->y / sc);
	NSPoint win = [hWnd->window convertPointFromScreen:scr];
	NSPoint v = [hWnd->view convertPoint:win fromView:nil];
	float s = Scale(hWnd);
	pt->x = (LONG)(v.x * s);
	pt->y = (LONG)(v.y * s);
	return TRUE;
}

WINBOOL ClientToScreen(HWND hWnd, POINT *pt)
{
	if (!hWnd || !hWnd->view) return FALSE;
	float s = Scale(hWnd);
	NSPoint v = NSMakePoint(pt->x / s, pt->y / s);
	NSPoint win = [hWnd->view convertPoint:v toView:nil];
	NSPoint scr = [hWnd->window convertPointToScreen:win];
	CGFloat sc = NSScreen.mainScreen.backingScaleFactor;
	pt->x = (LONG)(scr.x * sc);
	pt->y = (LONG)((MainScreenHeight() - scr.y) * sc);
	return TRUE;
}

WINBOOL GetClientRect(HWND hWnd, RECT *r)
{
	if (!hWnd || !hWnd->view) return FALSE;
	int w, h;
	oplat::GetDrawableSize(hWnd, &w, &h);
	r->left = r->top = 0;
	r->right = w;
	r->bottom = h;
	return TRUE;
}

WINBOOL GetWindowRect(HWND hWnd, RECT *r)
{
	if (!hWnd || !hWnd->window) return FALSE;
	NSRect f = hWnd->window.frame;
	CGFloat sc = hWnd->window.backingScaleFactor;
	r->left = (LONG)(f.origin.x * sc);
	r->top = (LONG)((MainScreenHeight() - f.origin.y - f.size.height) * sc);
	r->right = r->left + (LONG)(f.size.width * sc);
	r->bottom = r->top + (LONG)(f.size.height * sc);
	return TRUE;
}

WINBOOL IsWindow(HWND hWnd) { return hWnd && g_windows.count(hWnd) && hWnd->alive; }
WINBOOL IsWindowVisible(HWND hWnd) { return IsWindow(hWnd) && hWnd->window.isVisible; }

WINBOOL PostMessageA(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
	{
		std::lock_guard<std::mutex> lk(g_postedLock);
		g_posted.push_back({hWnd, msg, wParam, lParam});
	}
	if (![NSThread isMainThread]) oplat::WakeEventLoop();
	return TRUE;
}

LRESULT SendMessageA(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
	return Dispatch(hWnd, msg, wParam, lParam);
}

void PostQuitMessage(int code)
{
	std::lock_guard<std::mutex> lk(g_postedLock);
	g_posted.push_back({nullptr, WM_QUIT, (WPARAM)code, 0});
}

WINBOOL SetWindowTextA(HWND hWnd, const char *text)
{
	if (!IsWindow(hWnd)) return FALSE;
	hWnd->window.title = [NSString stringWithUTF8String:text ? text : ""];
	return TRUE;
}

HWND GetFocus(void)
{
	NSWindow *k = NSApp.keyWindow;
	for (HWND h : g_windows)
		if (h->window == k) return h;
	return nullptr;
}

HWND SetFocus(HWND hWnd)
{
	HWND prev = GetFocus();
	if (IsWindow(hWnd)) {
		[hWnd->window makeKeyWindow];
		[hWnd->window makeFirstResponder:hWnd->view];
	}
	return prev;
}

HWND GetForegroundWindow(void) { return GetFocus(); }

WINBOOL SetForegroundWindow(HWND hWnd)
{
	if (!IsWindow(hWnd)) return FALSE;
	[NSApp activateIgnoringOtherApps:YES];
	[hWnd->window makeKeyAndOrderFront:nil];
	return TRUE;
}

WINBOOL ShowWindow(HWND hWnd, int cmd)
{
	if (!IsWindow(hWnd)) return FALSE;
	BOOL was = hWnd->window.isVisible;
	switch (cmd) {
	case SW_HIDE: [hWnd->window orderOut:nil]; break;
	case SW_MINIMIZE: case SW_SHOWMINIMIZED: [hWnd->window miniaturize:nil]; break;
	case SW_MAXIMIZE: if (!hWnd->window.isZoomed) [hWnd->window zoom:nil]; [hWnd->window makeKeyAndOrderFront:nil]; break;
	case SW_RESTORE: if (hWnd->window.isMiniaturized) [hWnd->window deminiaturize:nil]; [hWnd->window makeKeyAndOrderFront:nil]; break;
	case SW_SHOWNA: case SW_SHOWNOACTIVATE: [hWnd->window orderFront:nil]; break;
	default: [hWnd->window makeKeyAndOrderFront:nil]; break;
	}
	return was;
}

WINBOOL DestroyWindow(HWND hWnd)
{
	if (!IsWindow(hWnd)) return FALSE;
	Dispatch(hWnd, WM_DESTROY, 0, 0);
	hWnd->alive = false;
	if (g_capture == hWnd) g_capture = nullptr;
	if (g_keyCapture == hWnd) g_keyCapture = nullptr;
	g_windows.erase(hWnd);
	@autoreleasepool {
		hWnd->window.delegate = nil;
		[hWnd->window orderOut:nil];
		[hWnd->window close];
		hWnd->view.owner = nullptr;
		hWnd->window = nil;
		hWnd->view = nil;
		hWnd->delegate = nil;
	}
	delete hWnd;
	return TRUE;
}

LRESULT DefWindowProcA(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
	if (msg == WM_CLOSE) DestroyWindow(hWnd);
	return 0;
}

LRESULT CallWindowProcA(WNDPROC proc, HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
	return proc ? proc(hWnd, msg, wParam, lParam) : DefWindowProcA(hWnd, msg, wParam, lParam);
}

HWND SetCapture(HWND hWnd)
{
	HWND prev = g_capture;
	g_capture = hWnd;
	return prev;
}

WINBOOL ReleaseCapture(void)
{
	g_capture = nullptr;
	return TRUE;
}

HWND GetCapture(void) { return g_capture; }

WINBOOL OpenClipboardText(char *buf, size_t size)
{
	@autoreleasepool {
		NSString *s = [[NSPasteboard generalPasteboard] stringForType:NSPasteboardTypeString];
		if (!s || !size) return FALSE;
		strncpy(buf, s.UTF8String, size - 1);
		buf[size - 1] = '\0';
		return TRUE;
	}
}

WINBOOL SetClipboardText(const char *text)
{
	@autoreleasepool {
		NSPasteboard *pb = [NSPasteboard generalPasteboard];
		[pb clearContents];
		return [pb setString:[NSString stringWithUTF8String:text ? text : ""] forType:NSPasteboardTypeString];
	}
}

}

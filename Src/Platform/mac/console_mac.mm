#import <AppKit/AppKit.h>
#include "OrbiterPlatform.h"
#include <deque>
#include <mutex>
#include <thread>
#include <unistd.h>
#include <iostream>

namespace {

std::mutex g_lock;
std::deque<std::string> g_lines;
oplat::ConsoleMode g_mode = oplat::CONSOLE_OUTPUT_ONLY;
bool g_open = false;
std::string g_status;

}

@interface OPConsoleController : NSObject <NSTextFieldDelegate, NSWindowDelegate>
@property (nonatomic, strong) NSWindow *window;
@property (nonatomic, strong) NSTextView *text;
@property (nonatomic, strong) NSTextField *input;
@property (nonatomic, strong) NSTextField *status;
@end

@implementation OPConsoleController

- (instancetype)initWithTitle:(NSString *)title
{
	if (!(self = [super init])) return nil;
	NSRect frame = NSMakeRect(0, 0, 760, 460);
	_window = [[NSWindow alloc] initWithContentRect:frame
		styleMask:NSWindowStyleMaskTitled | NSWindowStyleMaskMiniaturizable | NSWindowStyleMaskResizable
		backing:NSBackingStoreBuffered defer:NO];
	_window.title = title;
	_window.releasedWhenClosed = NO;
	_window.delegate = self;
	[_window center];

	NSView *content = _window.contentView;
	NSFont *mono = [NSFont monospacedSystemFontOfSize:12 weight:NSFontWeightRegular];

	NSScrollView *sv = [[NSScrollView alloc] init];
	sv.hasVerticalScroller = YES;
	sv.translatesAutoresizingMaskIntoConstraints = NO;
	_text = [[NSTextView alloc] initWithFrame:NSMakeRect(0, 0, frame.size.width, frame.size.height)];
	_text.editable = NO;
	_text.font = mono;
	_text.autoresizingMask = NSViewWidthSizable;
	_text.textContainerInset = NSMakeSize(6, 6);
	sv.documentView = _text;

	_status = [NSTextField wrappingLabelWithString:@""];
	_status.font = mono;
	_status.textColor = NSColor.secondaryLabelColor;
	_status.translatesAutoresizingMaskIntoConstraints = NO;
	_status.hidden = YES;

	_input = [NSTextField textFieldWithString:@""];
	_input.font = mono;
	_input.placeholderString = @"Type a command, or help";
	_input.translatesAutoresizingMaskIntoConstraints = NO;
	_input.target = self;
	_input.action = @selector(submit:);

	[content addSubview:sv];
	[content addSubview:_status];
	[content addSubview:_input];
	[NSLayoutConstraint activateConstraints:@[
		[sv.topAnchor constraintEqualToAnchor:content.topAnchor],
		[sv.leadingAnchor constraintEqualToAnchor:content.leadingAnchor],
		[sv.trailingAnchor constraintEqualToAnchor:content.trailingAnchor],
		[_status.topAnchor constraintEqualToAnchor:sv.bottomAnchor constant:6],
		[_status.leadingAnchor constraintEqualToAnchor:content.leadingAnchor constant:8],
		[_status.trailingAnchor constraintEqualToAnchor:content.trailingAnchor constant:-8],
		[_input.topAnchor constraintEqualToAnchor:_status.bottomAnchor constant:6],
		[_input.leadingAnchor constraintEqualToAnchor:content.leadingAnchor constant:8],
		[_input.trailingAnchor constraintEqualToAnchor:content.trailingAnchor constant:-8],
		[_input.bottomAnchor constraintEqualToAnchor:content.bottomAnchor constant:-8],
	]];
	[_window makeKeyAndOrderFront:nil];
	[_window makeFirstResponder:_input];
	return self;
}

- (BOOL)windowShouldClose:(NSWindow *)w
{
	std::lock_guard<std::mutex> lk(g_lock);
	g_lines.push_back("exit");
	return NO;
}

- (void)submit:(id)sender
{
	NSString *s = _input.stringValue;
	_input.stringValue = @"";
	[self append:[NSString stringWithFormat:@"> %@\n", s]];
	std::lock_guard<std::mutex> lk(g_lock);
	g_lines.push_back(s.UTF8String);
}

- (void)append:(NSString *)s
{
	NSDictionary *attr = @{ NSFontAttributeName: _text.font ?: [NSFont monospacedSystemFontOfSize:12 weight:NSFontWeightRegular],
		NSForegroundColorAttributeName: NSColor.textColor };
	[_text.textStorage appendAttributedString:[[NSAttributedString alloc] initWithString:s attributes:attr]];
	[_text scrollToEndOfDocument:nil];
}

@end

namespace {
OPConsoleController *g_console = nil;
}

namespace oplat {

bool StdinIsTerminal()
{
	return isatty(STDIN_FILENO) != 0;
}

void ConsoleOpen(const std::string &title, ConsoleMode mode)
{
	if (g_open) return;
	g_open = true;
	g_mode = mode;
	if (mode == CONSOLE_WINDOW) {
		@autoreleasepool {
			g_console = [[OPConsoleController alloc] initWithTitle:[NSString stringWithUTF8String:title.c_str()]];
		}
	} else if (mode == CONSOLE_TERMINAL) {
		static bool readerStarted = false;
		if (!readerStarted) {
			readerStarted = true;
			std::thread([] {
				std::string line;
				while (std::getline(std::cin, line)) {
					std::lock_guard<std::mutex> lk(g_lock);
					g_lines.push_back(line);
					oplat::WakeEventLoop();
				}
			}).detach();
		}
		std::cout << "> " << std::flush;
	}
}

void ConsoleClose()
{
	if (!g_open) return;
	g_open = false;
	if (g_console) {
		[g_console.window orderOut:nil];
		g_console = nil;
	}
	std::lock_guard<std::mutex> lk(g_lock);
	g_lines.clear();
}

void ConsoleWrite(const std::string &text)
{
	if (g_mode == CONSOLE_WINDOW && g_console) {
		@autoreleasepool {
			[g_console append:[NSString stringWithFormat:@"%s\n", text.c_str()]];
		}
		return;
	}
	if (g_mode == CONSOLE_TERMINAL) std::cout << "\r" << text << "\n> " << std::flush;
	else std::cout << text << std::endl;
}

bool ConsolePoll(std::string &line)
{
	std::lock_guard<std::mutex> lk(g_lock);
	if (g_lines.empty()) return false;
	line = g_lines.front();
	g_lines.pop_front();
	return true;
}

void ConsoleSetStatus(const std::string &status)
{
	if (g_mode == CONSOLE_WINDOW && g_console) {
		g_console.status.stringValue = [NSString stringWithUTF8String:status.c_str()];
		g_console.status.hidden = status.empty();
	} else if (!status.empty() && status != g_status) {
		if (g_mode == CONSOLE_TERMINAL) std::cout << "\r" << status << "\n> " << std::flush;
		else std::cout << status << std::endl;
	}
	g_status = status;
}

}

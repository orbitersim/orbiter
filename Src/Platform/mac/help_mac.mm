#import <AppKit/AppKit.h>
#import <WebKit/WebKit.h>
#include "OrbiterPlatform.h"
#include <fstream>
#include <sstream>

@interface OPTocItem : NSObject
@property (nonatomic, copy) NSString *name;
@property (nonatomic, copy) NSString *local;
@property (nonatomic, strong) NSMutableArray<OPTocItem *> *children;
@end

@implementation OPTocItem
- (instancetype)init
{
	if ((self = [super init])) _children = [NSMutableArray array];
	return self;
}
@end

namespace {

NSString *AttrValue(NSString *tag, NSString *attr)
{
	NSString *pattern = [NSString stringWithFormat:@"%@\\s*=\\s*\"([^\"]*)\"", attr];
	NSRegularExpression *re = [NSRegularExpression regularExpressionWithPattern:pattern options:NSRegularExpressionCaseInsensitive error:nil];
	NSTextCheckingResult *m = [re firstMatchInString:tag options:0 range:NSMakeRange(0, tag.length)];
	return m ? [tag substringWithRange:[m rangeAtIndex:1]] : nil;
}

OPTocItem *ParseToc(NSString *path)
{
	NSData *data = [NSData dataWithContentsOfFile:path];
	if (!data) return nil;
	NSString *s = [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding];
	if (!s) s = [[NSString alloc] initWithData:data encoding:NSWindowsCP1252StringEncoding];
	if (!s) return nil;

	OPTocItem *root = [OPTocItem new];
	NSMutableArray<OPTocItem *> *stack = [NSMutableArray arrayWithObject:root];
	OPTocItem *last = nil, *cur = nil;
	NSRegularExpression *tagRe = [NSRegularExpression regularExpressionWithPattern:@"<(/?)(\\w+)([^>]*)>" options:0 error:nil];
	for (NSTextCheckingResult *m in [tagRe matchesInString:s options:0 range:NSMakeRange(0, s.length)]) {
		BOOL close = [m rangeAtIndex:1].length > 0;
		NSString *name = [[s substringWithRange:[m rangeAtIndex:2]] lowercaseString];
		NSString *attrs = [s substringWithRange:[m rangeAtIndex:3]];
		if ([name isEqualToString:@"ul"]) {
			if (close) {
				if (stack.count > 1) [stack removeLastObject];
			} else if (last && stack.count) {
				[stack addObject:last];
			}
		} else if ([name isEqualToString:@"object"]) {
			if (!close) cur = [OPTocItem new];
			else if (cur) {
				if (cur.name.length) {
					[stack.lastObject.children addObject:cur];
					last = cur;
				}
				cur = nil;
			}
		} else if ([name isEqualToString:@"param"] && cur) {
			NSString *pn = AttrValue(attrs, @"name"), *pv = AttrValue(attrs, @"value");
			if ([pn caseInsensitiveCompare:@"Name"] == NSOrderedSame) cur.name = pv;
			else if ([pn caseInsensitiveCompare:@"Local"] == NSOrderedSame) cur.local = pv;
		}
	}
	return root;
}

}

@interface OPHelpController : NSObject <NSOutlineViewDataSource, NSOutlineViewDelegate, WKNavigationDelegate, NSWindowDelegate>
@property (nonatomic, strong) NSWindow *window;
@property (nonatomic, strong) NSOutlineView *outline;
@property (nonatomic, strong) WKWebView *web;
@property (nonatomic, strong) NSSplitView *split;
@property (nonatomic, strong) NSSegmentedControl *nav;
@property (nonatomic, strong) OPTocItem *toc;
@property (nonatomic, copy) NSString *dir;
@end

@implementation OPHelpController

- (instancetype)init
{
	if (!(self = [super init])) return nil;
	NSRect frame = NSMakeRect(0, 0, 980, 680);
	_window = [[NSWindow alloc] initWithContentRect:frame
		styleMask:NSWindowStyleMaskTitled | NSWindowStyleMaskClosable | NSWindowStyleMaskMiniaturizable | NSWindowStyleMaskResizable
		backing:NSBackingStoreBuffered defer:NO];
	_window.releasedWhenClosed = NO;
	_window.delegate = self;
	_window.tabbingMode = NSWindowTabbingModeDisallowed;
	[_window center];
	_window.frameAutosaveName = @"OrbiterHelp";

	NSScrollView *sv = [[NSScrollView alloc] initWithFrame:NSMakeRect(0, 0, 260, frame.size.height)];
	sv.hasVerticalScroller = YES;
	_outline = [[NSOutlineView alloc] initWithFrame:sv.bounds];
	NSTableColumn *col = [[NSTableColumn alloc] initWithIdentifier:@"name"];
	col.resizingMask = NSTableColumnAutoresizingMask;
	[_outline addTableColumn:col];
	_outline.outlineTableColumn = col;
	_outline.headerView = nil;
	_outline.style = NSTableViewStyleSourceList;
	_outline.dataSource = self;
	_outline.delegate = self;
	_outline.target = self;
	_outline.action = @selector(selectItem:);
	sv.documentView = _outline;

	WKWebViewConfiguration *cfg = [WKWebViewConfiguration new];
	_web = [[WKWebView alloc] initWithFrame:NSMakeRect(0, 0, 720, frame.size.height) configuration:cfg];
	_web.navigationDelegate = self;

	_split = [[NSSplitView alloc] initWithFrame:frame];
	_split.vertical = YES;
	_split.dividerStyle = NSSplitViewDividerStyleThin;
	[_split addSubview:sv];
	[_split addSubview:_web];
	_window.contentView = _split;

	NSToolbar *tb = [[NSToolbar alloc] initWithIdentifier:@"help"];
	_window.toolbar = tb;
	_nav = [NSSegmentedControl segmentedControlWithImages:@[
		[NSImage imageWithSystemSymbolName:@"chevron.left" accessibilityDescription:@"Back"],
		[NSImage imageWithSystemSymbolName:@"chevron.right" accessibilityDescription:@"Forward"]]
		trackingMode:NSSegmentSwitchTrackingMomentary target:self action:@selector(navigate:)];
	NSTitlebarAccessoryViewController *acc = [NSTitlebarAccessoryViewController new];
	acc.view = _nav;
	acc.layoutAttribute = NSLayoutAttributeLeading;
	[_window addTitlebarAccessoryViewController:acc];
	return self;
}

- (void)navigate:(NSSegmentedControl *)c
{
	if (c.selectedSegment == 0) [_web goBack];
	else [_web goForward];
}

- (void)updateNav
{
	[_nav setEnabled:_web.canGoBack forSegment:0];
	[_nav setEnabled:_web.canGoForward forSegment:1];
}

- (void)webView:(WKWebView *)w didFinishNavigation:(WKNavigation *)n
{
	[self updateNav];
}

- (void)webView:(WKWebView *)w decidePolicyForNavigationAction:(WKNavigationAction *)a decisionHandler:(void (^)(WKNavigationActionPolicy))h
{
	NSURL *u = a.request.URL;
	if (u && !u.isFileURL && a.navigationType == WKNavigationTypeLinkActivated) {
		[[NSWorkspace sharedWorkspace] openURL:u];
		h(WKNavigationActionPolicyCancel);
		return;
	}
	h(WKNavigationActionPolicyAllow);
}

- (void)showDir:(NSString *)dir page:(NSString *)page toc:(NSString *)tocPath title:(NSString *)title
{
	if (![dir isEqualToString:_dir]) {
		_dir = [dir copy];
		_toc = tocPath.length ? ParseToc(tocPath) : nil;
		[_outline reloadData];
		for (OPTocItem *i in _toc.children) [_outline expandItem:i];
		_split.subviews[0].hidden = (_toc == nil);
		[_split adjustSubviews];
		if (_toc) [_split setPosition:260 ofDividerAtIndex:0];
	}
	_window.title = title.length ? title : @"Help";
	NSString *anchor = nil;
	NSRange h = [page rangeOfString:@"#"];
	if (h.location != NSNotFound) {
		anchor = [page substringFromIndex:h.location + 1];
		page = [page substringToIndex:h.location];
	}
	NSString *file = [dir stringByAppendingPathComponent:page];
	if (![[NSFileManager defaultManager] fileExistsAtPath:file]) {
		NSString *lower = [page lowercaseString];
		for (NSString *f in [[NSFileManager defaultManager] contentsOfDirectoryAtPath:dir error:nil])
			if ([[f lowercaseString] isEqualToString:lower]) { file = [dir stringByAppendingPathComponent:f]; break; }
	}
	NSURLComponents *c = [NSURLComponents componentsWithURL:[NSURL fileURLWithPath:file] resolvingAgainstBaseURL:NO];
	if (anchor) c.fragment = anchor;
	[_web loadFileURL:c.URL allowingReadAccessToURL:[NSURL fileURLWithPath:[dir stringByDeletingLastPathComponent]]];
	[_window makeKeyAndOrderFront:nil];
	[NSApp activateIgnoringOtherApps:YES];
}

- (void)selectItem:(id)sender
{
	OPTocItem *i = [_outline itemAtRow:_outline.clickedRow];
	if (!i.local.length) {
		if ([_outline isItemExpanded:i]) [_outline collapseItem:i];
		else [_outline expandItem:i];
		return;
	}
	[self showDir:_dir page:i.local toc:nil title:_window.title];
}

- (NSInteger)outlineView:(NSOutlineView *)v numberOfChildrenOfItem:(id)item
{
	return ((OPTocItem *)(item ?: _toc)).children.count;
}

- (id)outlineView:(NSOutlineView *)v child:(NSInteger)index ofItem:(id)item
{
	return ((OPTocItem *)(item ?: _toc)).children[index];
}

- (BOOL)outlineView:(NSOutlineView *)v isItemExpandable:(id)item
{
	return ((OPTocItem *)item).children.count > 0;
}

- (NSView *)outlineView:(NSOutlineView *)v viewForTableColumn:(NSTableColumn *)col item:(id)item
{
	NSTableCellView *cell = [v makeViewWithIdentifier:@"cell" owner:self];
	if (!cell) {
		cell = [[NSTableCellView alloc] initWithFrame:NSMakeRect(0, 0, 200, 20)];
		cell.identifier = @"cell";
		NSTextField *tf = [NSTextField labelWithString:@""];
		tf.lineBreakMode = NSLineBreakByTruncatingTail;
		tf.translatesAutoresizingMaskIntoConstraints = NO;
		[cell addSubview:tf];
		cell.textField = tf;
		[NSLayoutConstraint activateConstraints:@[
			[tf.leadingAnchor constraintEqualToAnchor:cell.leadingAnchor constant:2],
			[tf.trailingAnchor constraintEqualToAnchor:cell.trailingAnchor constant:-2],
			[tf.centerYAnchor constraintEqualToAnchor:cell.centerYAnchor]]];
	}
	cell.textField.stringValue = ((OPTocItem *)item).name ?: @"";
	return cell;
}

@end

namespace {
OPHelpController *g_help = nil;
}

namespace oplat {

void ShowHelp(const std::string &dir, const std::string &page, const std::string &toc, const std::string &title)
{
	@autoreleasepool {
		if (!g_help) g_help = [OPHelpController new];
		NSString *d = [[NSString stringWithUTF8String:dir.c_str()] stringByStandardizingPath];
		if (![d isAbsolutePath])
			d = [[[NSFileManager defaultManager] currentDirectoryPath] stringByAppendingPathComponent:d];
		NSString *t = toc.empty() ? nil : [NSString stringWithUTF8String:toc.c_str()];
		if (t && ![t isAbsolutePath])
			t = [[[NSFileManager defaultManager] currentDirectoryPath] stringByAppendingPathComponent:t];
		[g_help showDir:d page:[NSString stringWithUTF8String:page.c_str()] toc:t title:[NSString stringWithUTF8String:title.c_str()]];
	}
}

}

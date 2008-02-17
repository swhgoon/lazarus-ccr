{
	NSWindow.h
	Application Kit
	Copyright (c) 1994-2005, Apple Computer, Inc.
	All rights reserved.
}

{#import <Foundation/NSGeometry.h>
#import <AppKit/NSResponder.h>
#import <AppKit/NSGraphics.h>
#import <AppKit/AppKitDefines.h>
#import <Foundation/NSDate.h>
#import <ApplicationServices/ApplicationServices.h>

@class NSButton, NSButtonCell, NSColor, NSImage, NSPasteboard, NSScreen;
@class NSNotification, NSText, NSView, NSMutableSet, NSSet, NSDate;
@class NSToolbar, NSGraphicsContext;}

const
  NSAppKitVersionNumberWithCustomSheetPosition = 686.0;

  NSBorderlessWindowMask   	        = 0;
  NSTitledWindowMask		        = 1 shl 0;
  NSClosableWindowMask	 	        = 1 shl 1;
  NSMiniaturizableWindowMask 	        = 1 shl 2;
  NSResizableWindowMask		        = 1 shl 3;

//#if MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_2
{ Specifies a window with textured background (eg. metal)
}
  NSTexturedBackgroundWindowMask	= 1 shl 8;
//#endif

//#if MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4
{ Specifies a window that ignores the userSpaceScaleFactor of the NSScreen on which it is created.  Currently restricted to borderless windows (NSBorderlessWindowMask)
}
    NSUnscaledWindowMask		= 1 shl 11;

{ Specifies a window whose titlebar and toolbar have a unified look - that is, a continuous background
}
    NSUnifiedTitleAndToolbarWindowMask	= 1 shl 12;

//#endif

{ used with NSRunLoop's performSelector:target:argument:order:modes: }
    NSDisplayWindowRunLoopOrdering	= 600000;
    NSResetCursorRectsRunLoopOrdering	= 700000;

{#define NSNormalWindowLevel              kCGNormalWindowLevel
#define NSFloatingWindowLevel		 kCGFloatingWindowLevel
#define NSSubmenuWindowLevel		 kCGTornOffMenuWindowLevel
#define NSTornOffMenuWindowLevel	 kCGTornOffMenuWindowLevel
#define NSMainMenuWindowLevel		 kCGMainMenuWindowLevel
#define NSStatusWindowLevel		 kCGStatusWindowLevel
// NSDockWindowLevel is deprecated
#define NSDockWindowLevel		 kCGDockWindowLevel
#define NSModalPanelWindowLevel          kCGModalPanelWindowLevel
#define NSPopUpMenuWindowLevel           kCGPopUpMenuWindowLevel
#define NSScreenSaverWindowLevel         kCGScreenSaverWindowLevel  }

const
  Str_NSWindow = 'NSWindow';
  Str_initWithContentRect = 'initWithContentRect:styleMask:backing:defer:';
  Str_orderFrontRegardless = 'orderFrontRegardless';

{ Notifications }
{APPKIT_EXTERN NSString *NSWindowDidBecomeKeyNotification;
APPKIT_EXTERN NSString *NSWindowDidBecomeMainNotification;
APPKIT_EXTERN NSString *NSWindowDidChangeScreenNotification;
APPKIT_EXTERN NSString *NSWindowDidDeminiaturizeNotification;
APPKIT_EXTERN NSString *NSWindowDidExposeNotification;			// userInfo key:  @"NSExposedRect"
APPKIT_EXTERN NSString *NSWindowDidMiniaturizeNotification;
APPKIT_EXTERN NSString *NSWindowDidMoveNotification;
APPKIT_EXTERN NSString *NSWindowDidResignKeyNotification;
APPKIT_EXTERN NSString *NSWindowDidResignMainNotification;
APPKIT_EXTERN NSString *NSWindowDidResizeNotification;
APPKIT_EXTERN NSString *NSWindowDidUpdateNotification;
APPKIT_EXTERN NSString *NSWindowWillCloseNotification;
APPKIT_EXTERN NSString *NSWindowWillMiniaturizeNotification;
APPKIT_EXTERN NSString *NSWindowWillMoveNotification;
APPKIT_EXTERN NSString *NSWindowWillBeginSheetNotification;
APPKIT_EXTERN NSString *NSWindowDidEndSheetNotification;
APPKIT_EXTERN NSString *NSWindowDidChangeScreenProfileNotification  AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER;
}


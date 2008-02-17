{
	NSApplication.h
	Application Kit
	Copyright (c) 1994-2005, Apple Computer, Inc.
	All rights reserved.
}

//#import <AppKit/NSResponder.h>
//#import <AppKit/AppKitDefines.h>

//@class NSDate, NSDictionary, NSError, NSException, NSNotification;
//@class NSGraphicsContext, NSImage, NSPasteboard, NSWindow;

{ The version of the AppKit framework }
//APPKIT_EXTERN const double NSAppKitVersionNumber;

const
  NSAppKitVersionNumber10_0 = 577;
  NSAppKitVersionNumber10_1 = 620;
  NSAppKitVersionNumber10_2 = 663;
  NSAppKitVersionNumber10_2_3 = 663.6;
  NSAppKitVersionNumber10_3 = 743;
  NSAppKitVersionNumber10_3_2 = 743.14;
  NSAppKitVersionNumber10_3_3 = 743.2;
  NSAppKitVersionNumber10_3_5 = 743.24;

{ Modes passed to NSRunLoop }
//APPKIT_EXTERN NSString *NSModalPanelRunLoopMode;
//APPKIT_EXTERN NSString *NSEventTrackingRunLoopMode;

{ Pre-defined return values for runModalFor: and runModalSession:. The system also reserves all values below these. }
const
    NSRunStoppedResponse			= (-1000);
    NSRunAbortedResponse			= (-1001);
    NSRunContinuesResponse     	        	= (-1002);

{ used with NSRunLoop's performSelector:target:argument:order:modes: }
const
    NSUpdateWindowsRunLoopOrdering		= 500000;

{ Information used by the system during modal sessions }
//typedef struct _NSModalSession *NSModalSession;
// threading information
//typedef struct NSThreadPrivate _NSThreadPrivate;

const
  Str_NSApplication = 'NSApplication';
  
  Str_sharedApplication = 'sharedApplication';
  Str_run = 'run';
  Str_runModalForWindow = 'runModalForWindow:';

  Str_orderFrontStandardAboutPanelWithOptions = 'orderFrontStandardAboutPanelWithOptions:';

{ An Application's startup function }

//APPKIT_EXTERN int NSApplicationMain(int argc, const char *argv[]);

//#if MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_2
{ The startup function to call for a Cocoa bundle }

function NSApplicationLoad(): CBOOL; cdecl; external;
//#endif

{ NSShowsServicesMenuItem() always returns YES. }
//APPKIT_EXTERN BOOL NSShowsServicesMenuItem(NSString * itemName);

{ NSSetShowsServicesMenuItem() has no effect, and always returns 0. }
//APPKIT_EXTERN int NSSetShowsServicesMenuItem(NSString * itemName, BOOL enabled);

{ NSUpdateDynamicServices() causes the services information for the system to be updated.  This will only be necessary if your program adds dynamic services to the system (i.e. services not found in macho segments of executables).
}
{APPKIT_EXTERN void NSUpdateDynamicServices(void);
APPKIT_EXTERN BOOL NSPerformService(NSString *itemName, NSPasteboard *pboard);

APPKIT_EXTERN void NSRegisterServicesProvider(id provider, NSString *name); // apps should use -setServicesProvider
APPKIT_EXTERN void NSUnregisterServicesProvider(NSString *name);   }

{ Notifications }
{APPKIT_EXTERN NSString *NSApplicationDidBecomeActiveNotification;
APPKIT_EXTERN NSString *NSApplicationDidHideNotification;
APPKIT_EXTERN NSString *NSApplicationDidFinishLaunchingNotification;
APPKIT_EXTERN NSString *NSApplicationDidResignActiveNotification;
APPKIT_EXTERN NSString *NSApplicationDidUnhideNotification;
APPKIT_EXTERN NSString *NSApplicationDidUpdateNotification;
APPKIT_EXTERN NSString *NSApplicationWillBecomeActiveNotification;
APPKIT_EXTERN NSString *NSApplicationWillHideNotification;
APPKIT_EXTERN NSString *NSApplicationWillFinishLaunchingNotification;
APPKIT_EXTERN NSString *NSApplicationWillResignActiveNotification;
APPKIT_EXTERN NSString *NSApplicationWillUnhideNotification;
APPKIT_EXTERN NSString *NSApplicationWillUpdateNotification;
APPKIT_EXTERN NSString *NSApplicationWillTerminateNotification;
APPKIT_EXTERN NSString *NSApplicationDidChangeScreenParametersNotification;   }


{%mainunit appkit.pas}
{
        NSStatusItem.h
        Application Kit
        Copyright (c) 1997-2005, Apple Computer, Inc.
        All rights reserved.
}

{$ifdef HEADER}
{$ifndef NSSTATUSITEM_PAS_H}
{$define NSSTATUSITEM_PAS_H}

{#import <Foundation/NSObject.h>
#import <Foundation/NSGeometry.h>}

(*typedef struct {
    unsigned int customView:1;
    unsigned int highlightMode:1;
    unsigned int hasAlternateImage:1;
    unsigned int reserved:29;
} _SBIFlags; *)

{ Class and method name strings }
const
  Str_NSStatusItem = 'NSStatusItem';

  Str_setImage = 'setImage:';

{$endif}
{$endif}
{$ifdef CLASSES}
{$ifndef NSSTATUSITEM_PAS_C}
{$define NSSTATUSITEM_PAS_C}

{@class NSAttributedString;
@class NSImage;
@class NSMenu;}
//  NSStatusBar = class;
{@class NSString;
@class NSView;
@class NSWindow;}

  NSStatusItem = class(NSObject)
  public
    { Extra binding functions }
    function getClass: objc.id; override;
  public

{- (NSStatusBar*) statusBar;

- (float) length;
- (void) setLength:(float)length;

@end

@interface NSStatusItem (NSStatusItemCommon)

- (SEL) action;
- (void) setAction:(SEL)action;

#if MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4
- (SEL) doubleAction;
- (void) setDoubleAction:(SEL)action;
#endif

- (id) target;
- (void) setTarget:(id)target;

- (NSString*) title;
- (void) setTitle:(NSString*)title;

- (NSAttributedString*) attributedTitle;
- (void) setAttributedTitle:(NSAttributedString*)title;

- (NSImage*) image;}
    procedure setImage(image: NSImage);

{#if MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3
- (NSImage *)alternateImage;
- (void)setAlternateImage:(NSImage*)image;
#endif

- (NSMenu*) menu;
- (void) setMenu:(NSMenu*)menu;

- (BOOL) isEnabled;
- (void) setEnabled:(BOOL)enabled;

- (NSString*) toolTip;
- (void) setToolTip:(NSString*)toolTip;

- (void) setHighlightMode:(BOOL) highlightMode;
- (BOOL) highlightMode;

- (void) sendActionOn:(int)mask;

#if MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3
- (void) popUpStatusItemMenu:(NSMenu*)menu;
- (void) drawStatusBarBackgroundInRect:(NSRect)rect withHighlight:(BOOL)highlight;
#endif

@end

@interface NSStatusItem (NSStatusItemView)

- (NSView*) view;
- (void) setView:(NSView*)view;}

  end;

{$endif}
{$endif}
{$ifdef IMPLEMENTATION}

function NSStatusItem.getClass: objc.id;
begin
  Result := objc_getClass(Str_NSStatusItem);
end;

procedure NSStatusItem.setImage(image: NSImage);
type
  setImage_t = procedure (param1: objc.id; param2: SEL;
   param3: objc.id); cdecl;
var
  vmethod: setImage_t;
begin
  vmethod := setImage_t(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(Str_setImage)), image.Handle);
end;

{$endif}


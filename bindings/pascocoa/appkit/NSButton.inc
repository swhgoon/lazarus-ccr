{%mainunit appkit.pas}
{
	NSButton.h
	Application Kit
	Copyright (c) 1994-2005, Apple Computer, Inc.
	All rights reserved.
}

{$ifdef HEADER}
{$ifndef NSBUTTON_PAS_H}
{$define NSBUTTON_PAS_H}

const
  Str_NSButton = 'NSButton';

  Str_setTitle = 'setTitle:';
  Str_setBezelStyle = 'setBezelStyle:';

{$endif}
{$endif}
{$ifdef CLASSES}
{$ifndef NSBUTTON_PAS_C}
{$define NSBUTTON_PAS_C}

{$include NSControl.inc}
{$include NSButtonCell.inc}

//@class NSSound;

  NSButton = class(NSControl)
  public
    { Extra binding functions }
    function getClass: objc.id; override;
  public
//- (NSString *)title;
    procedure setTitle(aString: CFStringRef);
{- (NSString *)alternateTitle;
- (void)setAlternateTitle:(NSString *)aString;
- (NSImage *)image;
- (void)setImage:(NSImage *)image;
- (NSImage *)alternateImage;
- (void)setAlternateImage:(NSImage *)image;
- (NSCellImagePosition)imagePosition;
- (void)setImagePosition:(NSCellImagePosition)aPosition;
- (void)setButtonType:(NSButtonType)aType;
- (int)state;
- (void)setState:(int)value;
- (BOOL)isBordered;
- (void)setBordered:(BOOL)flag;
- (BOOL)isTransparent;
- (void)setTransparent:(BOOL)flag;
- (void)setPeriodicDelay:(float)delay interval:(float)interval;
- (void)getPeriodicDelay:(float *)delay interval:(float *)interval;
- (NSString *)keyEquivalent;
- (void)setKeyEquivalent:(NSString *)charCode;
- (unsigned int)keyEquivalentModifierMask;
- (void)setKeyEquivalentModifierMask:(unsigned int)mask;
- (void)highlight:(BOOL)flag;
- (BOOL)performKeyEquivalent:(NSEvent *)key;

@end

@interface NSButton(NSKeyboardUI)
- (void)setTitleWithMnemonic:(NSString *)stringWithAmpersand;
@end

@interface NSButton(NSButtonAttributedStringMethods)
- (NSAttributedString *)attributedTitle;
- (void)setAttributedTitle:(NSAttributedString *)aString;
- (NSAttributedString *)attributedAlternateTitle;
- (void)setAttributedAlternateTitle:(NSAttributedString *)obj;
@end}

//@interface NSButton(NSButtonBezelStyles)
    procedure setBezelStyle(bezelStyle: NSBezelStyle);
//- (NSBezelStyle)bezelStyle;
//@end

{@interface NSButton(NSButtonMixedState)
- (void)setAllowsMixedState:(BOOL)flag;
- (BOOL)allowsMixedState;
- (void)setNextState;
@end

@interface NSButton(NSButtonBorder)
- (void) setShowsBorderOnlyWhileMouseInside:(BOOL)show;
- (BOOL) showsBorderOnlyWhileMouseInside;
@end

@interface NSButton (NSButtonSoundExtensions)
- (void)setSound:(NSSound *)aSound;
- (NSSound *)sound;
@end}

  end;

{$endif}
{$endif}
{$ifdef IMPLEMENTATION}

function NSButton.getClass: objc.id;
begin
  Result := objc_getClass(Str_NSButton);
end;

procedure NSButton.setTitle(aString: CFStringRef);
type
  setTitle_t = procedure (param1: objc.id; param2: SEL;
   param3: CFStringRef); cdecl;
var
  vmethod: setTitle_t;
begin
  vmethod := setTitle_t(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(Str_setTitle)), aString);
end;

procedure NSButton.setBezelStyle(bezelStyle: NSBezelStyle);
type
  setBezelStyle_t = procedure (param1: objc.id; param2: SEL;
   param3: NSBezelStyle); cdecl;
var
  vmethod: setBezelStyle_t;
begin
  vmethod := setBezelStyle_t(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(Str_setBezelStyle)), bezelStyle);
end;

{$endif}


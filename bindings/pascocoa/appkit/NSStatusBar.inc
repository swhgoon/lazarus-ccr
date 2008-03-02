{%mainunit appkit.pas}
{
        NSStatusBar.h
        Application Kit
        Copyright (c) 1997-2005, Apple Computer, Inc.
        All rights reserved.
}

{$ifdef HEADER}
{$ifndef NSSTATUSBAR_PAS_H}
{$define NSSTATUSBAR_PAS_H}

const
  NSVariableStatusItemLength = (-1);
  NSSquareStatusItemLength   = (-2);

{ Class and method name strings }
const
  Str_NSStatusBar = 'NSStatusBar';

  Str_systemStatusBar = 'systemStatusBar';

{$endif}
{$endif}
{$ifdef CLASSES}
{$ifndef NSSTATUSBAR_PAS_C}
{$define NSSTATUSBAR_PAS_C}

{@class NSColor;
@class NSFont;}
  NSStatusItem = class;
{@class NSMutableArray;}

  NSStatusBar = class(NSObject)
  public
    constructor systemStatusBar();

    function statusItemWithLength(length: cfloat): NSStatusItem;
    procedure removeStatusItem(item: NSStatusItem);

{- (BOOL)     isVertical;
- (float)    thickness;}

  end;

{$endif}
{$endif}
{$ifdef IMPLEMENTATION}

constructor NSStatusBar.systemStatusBar();
begin
  ClassId := objc_getClass(PChar(Str_NSStatusBar));
  Handle := objc_msgSend(ClassId, sel_registerName(PChar(Str_systemStatusBar)), []);
end;

function NSStatusBar.statusItemWithLength(length: cfloat): NSStatusItem;
begin

end;

procedure NSStatusBar.removeStatusItem(item: NSStatusItem);
begin

end;

{$endif}


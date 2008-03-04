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
  Str_statusItemWithLength = 'statusItemWithLength:';

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
    { Extra binding functions }
    function getClass: objc.id; override;
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

function NSStatusBar.getClass: objc.id;
begin
  Result := objc_getClass(Str_NSStatusBar);
end;

constructor NSStatusBar.systemStatusBar();
begin
  ClassId := getClass();
  Handle := objc_msgSend(ClassId, sel_registerName(PChar(Str_systemStatusBar)), []);
end;

function NSStatusBar.statusItemWithLength(length: cfloat): NSStatusItem;
type
  statusItemWithLength_t = function (param1: objc.id; param2: SEL;
   param3: cfloat): objc.id; cdecl;
var
  vmethod: statusItemWithLength_t;
  itemHandle: objc.id;
begin
  vmethod := statusItemWithLength_t(@objc_msgSend);
  itemHandle := vmethod(Handle, sel_registerName(PChar(Str_statusItemWithLength)), length);
  Result := NSStatusItem.CreateWithHandle(itemHandle);
end;

procedure NSStatusBar.removeStatusItem(item: NSStatusItem);
begin

end;

{$endif}


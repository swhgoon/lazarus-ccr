{
 actions.pas

 This example project is released under public domain

 AUTHORS: Felipe Monteiro de Carvalho
}
unit actions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, foundation, objc, appkit, FPCMacOSAll;

type

  { TMyActionList }

  TMyActionList = class(NSObject)
  public
    { Extra binding functions }
    constructor Create; override;
    procedure AddMethods;
    { Fields }
    bar: NSStatusBar;
    item: NSStatusItem;
    image: NSImage;
  end;

{ Objective-c Methods }
procedure doShowStatusitem(param1: objc.id; param2: SEL; param3: objc.id); cdecl;

var
  actionList: TMyActionList;

implementation

{ TMyActionList }

procedure TMyActionList.AddMethods;
var
  method_list: Pobjc_method_list;
begin
  { Adds methods to the class }

  method_list := GetMem(SizeOf(objc_method_list)); { We can't free this until the last instance is freed }

  method_list^.method_count := 1;
  method_list^.method_list[0].method_name := sel_registerName('doShowStatusitem:');
  { The first parameter is the result (v = void),
    followed by self and _cmd (@ = id and : = SEL),
    and on the end "sender" (@ = id) }
  method_list^.method_list[0].method_types := 'v@:@';
  method_list^.method_list[0].method_imp := IMP(@doShowStatusitem);
  class_addMethods(ClassId, method_list);
end;

constructor TMyActionList.Create;
var
  fileName: CFStringRef;
begin
  inherited Create;

  AddMethods();

  bar := NSStatusBar.systemStatusBar();
  
  fileName := CFStringCreateWithPascalString(nil,
   ExtractFilePath(ParamStr(0)) + 'icon.ico', kCFStringEncodingUTF8);
  image := NSImage.initWithContentsOfFile(fileName);
end;

{ Objective-c Methods }

procedure doShowStatusitem(param1: objc.id; param2: SEL; param3: objc.id); cdecl;
begin
  actionList.item := actionList.bar.statusItemWithLength(NSSquareStatusItemLength);

  if actionList.item = nil then WriteLn('The item is nil!');
  if actionList.image = nil then WriteLn('The image is nil!');

  actionList.item.retain();
  actionList.item.setImage(actionList.image);
end;

end.


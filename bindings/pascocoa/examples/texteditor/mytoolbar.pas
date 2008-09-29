unit mytoolbar;

{$mode delphi}{$STATIC ON}

interface

uses
  MacOSAll, objc, appkit, foundation;

type

  { TMyToolbarController }

  TMyToolbarController = class(NSObject)
  public
    { Extra binding functions }
    constructor Create; override;
    procedure AddMethods; override;
    { Objective-c Methods }
//    class procedure doClose(_self: objc.id; _cmd: SEL; sender: objc.id); cdecl; //static;
    class function toolbarAllowedItemIdentifiers(_self: objc.id;
     _cmd: SEL; toolbar: objc.id {NSToolbar}): CFArrayRef; cdecl;// static;
    class function toolbarDefaultItemIdentifiers(_self: objc.id;
     _cmd: SEL; toolbar: objc.id {NSToolbar}): CFArrayRef; cdecl;// static;
    class function toolbar_itemForItemIdentifier_willBeInsertedIntoToolbar
      (_self: objc.id; _cmd: SEL; toolbar: objc.id;
      itemIdentifier: CFStringRef; flag: CBOOL): objc.id; cdecl;// static;
  end;

const
  Str_toolbarAllowedItemIdentifiers = 'toolbarAllowedItemIdentifiers:';
  Str_toolbarDefaultItemIdentifiers = 'toolbarDefaultItemIdentifiers:';
  Str_toolbar_itemForItemIdentifier_willBeInsertedIntoToolbar = 'toolbar:itemForItemIdentifier:willBeInsertedIntoToolbar:';

var
  OpenToolbarItemIdentifier, SaveToolbarItemIdentifier, CloseToolbarItemIdentifier: CFStringRef;

implementation

uses
  model, controller;

{ TMyToolbar }

constructor TMyToolbarController.Create;
begin
  { The class is registered on the Objective-C runtime before the NSObject constructor is called }
  if not CreateClassDefinition(ClassName(), Str_NSObject) then WriteLn('Failed to create objc class ' + ClassName());

  inherited Create;

  { Prepare CFStringRefs for the constants }

  OpenToolbarItemIdentifier := CFStringCreateWithPascalString(nil, 'OpenID', kCFStringEncodingUTF8);
  SaveToolbarItemIdentifier := CFStringCreateWithPascalString(nil, 'SaveID', kCFStringEncodingUTF8);
  CloseToolbarItemIdentifier := CFStringCreateWithPascalString(nil, 'CloseID', kCFStringEncodingUTF8);
end;

procedure TMyToolbarController.AddMethods;
begin
  AddMethod(Str_toolbarAllowedItemIdentifiers, '@@:@', Pointer(toolbarAllowedItemIdentifiers));
  AddMethod(Str_toolbarDefaultItemIdentifiers, '@@:@', Pointer(toolbarDefaultItemIdentifiers));
//  AddMethod(Str_toolbar_itemForItemIdentifier_willBeInsertedIntoToolbar,
//   '@@:@@L', Pointer(toolbar_itemForItemIdentifier_willBeInsertedIntoToolbar));
end;

class function TMyToolbarController.toolbarAllowedItemIdentifiers(_self: objc.id;
  _cmd: SEL; toolbar: objc.id): CFArrayRef; cdecl;
var
  toolbarItems: array[0..4] of CFStringRef;
begin
  toolbarItems[0] := OpenToolbarItemIdentifier;
  toolbarItems[1] := SaveToolbarItemIdentifier;
//  toolbarItems[2] := NSToolbarSpaceItemIdentifier;
  toolbarItems[2] := CloseToolbarItemIdentifier;
  toolbarItems[3] := nil;
  toolbarItems[4] := nil;

  Result := CFArrayCreate(nil, @toolbarItems[0], 4, nil);
end;

class function TMyToolbarController.toolbarDefaultItemIdentifiers(_self: objc.id;
  _cmd: SEL; toolbar: objc.id): CFArrayRef; cdecl;
begin
  Result := toolbarAllowedItemIdentifiers(_self, _cmd, toolbar);
end;

class function TMyToolbarController.toolbar_itemForItemIdentifier_willBeInsertedIntoToolbar(_self: objc.id;
  _cmd: SEL; toolbar: objc.id {NSToolbar}; itemIdentifier: CFStringRef;
  flag: CBOOL): objc.id {NSToolbarItem}; cdecl;
var
  toolbarItem: NSToolbarItem;
begin
  if CFStringCompare(itemIdentifier, OpenToolbarItemIdentifier, kCFCompareCaseInsensitive) = kCFCompareEqualTo then
  begin
    toolbarItem := NSToolbarItem.initWithItemIdentifier(itemIdentifier);
//    [toolbarItem setLabel:@"Save"];
//   [toolbarItem setPaletteLabel:[toolbarItem label]];
//    [toolbarItem setToolTip:@"Save Your Passwords"];}
    toolbarItem.setImage(myModel.imgOpen.Handle);
    toolbarItem.setTarget(myController.Handle);
    toolbarItem.setAction(sel_registerName(PChar('doOpenFile:')));
  end
  else if CFStringCompare(itemIdentifier, SaveToolbarItemIdentifier, kCFCompareCaseInsensitive) = kCFCompareEqualTo then
  begin
    toolbarItem := NSToolbarItem.initWithItemIdentifier(itemIdentifier);

    toolbarItem.setImage(myModel.imgSave.Handle);
    toolbarItem.setTarget(myController.Handle);
    toolbarItem.setAction(sel_registerName(PChar('doSaveFile:')));
  end
  else if CFStringCompare(itemIdentifier, CloseToolbarItemIdentifier, kCFCompareCaseInsensitive) = kCFCompareEqualTo then
  begin
    toolbarItem := NSToolbarItem.initWithItemIdentifier(itemIdentifier);

    toolbarItem.setImage(myModel.imgClose.Handle);
    toolbarItem.setTarget(myController.Handle);
    toolbarItem.setAction(sel_registerName(PChar('doCloseFile:')));
  end
  else
  begin
    Result := nil;
    Exit
  end;

  Result := ToolbarItem.autorelease;
{  ToolbarItem.Handle := 0;
  ToolbarItem.Free;}
//    Result := Result. [toolbarItem autorelease];
end;

end.


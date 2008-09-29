unit view;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  MacOSAll, appkit, foundation, objc,
  mytoolbar;

type

  { TMyView }

  TMyView = class
  private
    { Other helper functions }
    function CreateToolbar(AOwnerView: NSView; AX, AY, AWidth, AHeight: Double): NSToolbar;
    function CreateMainMenu(): NSMenu;
    function CreateMenuItem(ATitle: shortstring;
     ACallbackName: string; ACallbackClass: NSObject): NSMenuItem;
    function CreateTextField(): NSTextField;
  public
    { classes }
    MainWindow: NSWindow;
    MainWindowView: NSView;
    Toolbar: NSToolbar;
    ToolbarController: TMyToolbarController;
    MainMenu: NSMenu;
    TextField: NSTextField;
    FileMenuItem, OpenItem, SaveItem, SaveAsItem, ExitItem: NSMenuItem;
    FileMenu: NSMenu;
    { strings and sizes}
    CFWindowTitle, CFEmptyString: CFStringRef;
    MainWindowRect: NSRect;
    MenuTitle: CFStringRef;
    { methods }
    procedure CreateUserInterface();
  end;

var
  myView: TMyView;

const
  Str_Window_Title = 'Text Editor';

implementation

uses controller, model;

{@@
}
procedure TMyView.CreateUserInterface();
begin
  CFEmptyString := CFStringCreateWithPascalString(nil, '', kCFStringEncodingUTF8);

  { Creates the main window }

  MainWindowRect.origin.x := 300.0;
  MainWindowRect.origin.y := 300.0;
  MainWindowRect.size.width := 300.0;
  MainWindowRect.size.height := 500.0;

  MainWindow := NSWindow.initWithContentRect_styleMask_backing_defer(MainWindowRect,
    NSTitledWindowMask or NSClosableWindowMask or NSMiniaturizableWindowMask or NSResizableWindowMask,
    NSBackingStoreBuffered, LongBool(NO));
  MainWindowView := NSView.CreateWithHandle(MainWindow.contentView);

  CFWindowTitle := CFStringCreateWithPascalString(nil, Str_Window_Title, kCFStringEncodingUTF8);
  MainWindow.setTitle(CFWindowTitle);

  { Adds the toolbar and it's buttons }

  Toolbar := CreateToolbar(MainWindowView, 0, MainWindowRect.size.height - 50, MainWindowRect.size.width, 50);

  MainWindow.setToolbar(Toolbar.Handle);

  { Add the text area }

  TextField := CreateTextField();

  { Add the main menu }

  MainMenu := CreateMainMenu();

  NSApp.setAppleMenu(MainMenu.Handle);
  NSApp.setMainMenu(MainMenu.Handle);
end;

{@@
}
function TMyView.CreateToolbar(AOwnerView: NSView; AX, AY, AWidth,
  AHeight: Double): NSToolbar;
begin
  Result := NSToolbar.initWithIdentifier(CFEmptyString);
  ToolbarController := TMyToolbarController.Create;
  Result.setDelegate(ToolbarController.Handle);
end;

{@@
}
function TMyView.CreateMainMenu(): NSMenu;
begin
  MenuTitle := CFStringCreateWithPascalString(nil, 'Title', kCFStringEncodingUTF8);
  WriteLn('CreateMenu');
  Result := NSMenu.initWithTitle(CFEmptyString);
  WriteLn('Menu Created');

  { Creates the file menu }

  FileMenu := NSMenu.initWithTitle(MenuTitle);
  FileMenuItem := NSMenuItem.initWithTitle_action_keyEquivalent(CFEmptyString, nil, CFEmptyString);
  FileMenuItem.setSubmenu(FileMenu.Handle);
  Result.addItem(FileMenuItem.Handle);

  { Adds items to it }

  OpenItem := CreateMenuItem('Open', 'doOpenFile:', myController);
  FileMenu.addItem(OpenItem.Handle);
  OpenItem := CreateMenuItem('Save', 'doSaveFile:', myController);
  FileMenu.addItem(OpenItem.Handle);
  OpenItem := CreateMenuItem('Save As', 'doSaveFileAs:', myController);
  FileMenu.addItem(OpenItem.Handle);
  OpenItem := CreateMenuItem('Exit', 'doClose:', myController);
  FileMenu.addItem(OpenItem.Handle);
end;

{@@
}
function TMyView.CreateMenuItem(ATitle: shortstring;
  ACallbackName: string; ACallbackClass: NSObject): NSMenuItem;
var
  ItemText: CFStringRef;
  KeyText: CFStringRef;
begin
  KeyText := CFStringCreateWithPascalString(nil, '', kCFStringEncodingUTF8);
  ItemText := CFStringCreateWithPascalString(nil, ATitle, kCFStringEncodingUTF8);
  WriteLn(' ItemText: ', IntToHex(Int64(ItemText), 8), ' ATitle: ', ATitle);

  Result := NSMenuItem.initWithTitle_action_keyEquivalent(ItemText, nil, KeyText);
  Result.setTarget(ACallbackClass.Handle);
  Result.setAction(sel_registerName(PChar(ACallbackName)));
end;

function TMyView.CreateTextField(): NSTextField;
var
  ClientRect: NSRect;
begin
  ClientRect.origin.x := 0.0;
  ClientRect.origin.y := 0.0;
  ClientRect.size.width := 300.0;
  ClientRect.size.height := 500.0;

  Result := NSTextField.initWithFrame(ClientRect);
  MainWindowView.addSubview(Result.Handle);
end;

end.


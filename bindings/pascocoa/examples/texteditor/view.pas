{
 view.pas

 View class of the texteditor example. Creates the user interface.

 This example project is released under public domain

 AUTHORS: Felipe Monteiro de Carvalho
}
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
    procedure AddToMenubar(menu: NSMenu);

    function CreateAppleMenu(): NSMenu;
    function CreateFileMenu(): NSMenu;
    procedure CreateMainMenu();
    function CreateMenuItem(ATitle: shortstring): NSMenuItem;

    function CreateToolbar(AOwnerView: NSView; AX, AY, AWidth, AHeight: Double): NSToolbar;
    function CreateTextField(): NSTextField;
  public
    { classes }
    MainWindow: NSWindow;
    MainWindowView: NSView;
    Toolbar: NSToolbar;
    TextField: NSTextField;
    MainMenu, AppleMenu, ServicesMenu, FileMenu: NSMenu;
    OpenItem, SaveItem, SaveAsItem, ExitItem: NSMenuItem;
    { strings and sizes}
    CFWindowTitle, CFEmptyString: CFStringRef;
    MainWindowRect: NSRect;
    { methods }
    procedure CreateUserInterface();
    procedure AttachEventHandlers();
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

  CreateMainMenu();
end;

{@@
}
procedure TMyView.AttachEventHandlers();
begin
  OpenItem.setTarget(myController.Handle);
  OpenItem.setAction(sel_registerName(PChar('doOpenFile:')));

  SaveItem.setTarget(myController.Handle);
  SaveItem.setAction(sel_registerName(PChar('doSaveFile:')));

  ExitItem.setTarget(myController.Handle);
  ExitItem.setAction(sel_registerName(PChar('doClose:')));
end;

{@@
}
function TMyView.CreateAppleMenu(): NSMenu;
var
  AppleMenuTitle, ServicesMenuTitle: CFStringRef;
begin
  AppleMenuTitle := CFStringCreateWithPascalString(nil, 'Apple Menu', kCFStringEncodingUTF8);
  ServicesMenuTitle := CFStringCreateWithPascalString(nil, 'Services', kCFStringEncodingUTF8);

  { Creates the Apple menu }

  Result := NSMenu.initWithTitle(AppleMenuTitle);

  { Add the services submenu }

  ServicesMenu := NSMenu.initWithTitle(ServicesMenuTitle);
  NSApp.setServicesMenu(ServicesMenu.Handle);
end;

{@@
}
function TMyView.CreateFileMenu(): NSMenu;
var
  MenuTitle: CFStringRef;
begin
  MenuTitle := CFStringCreateWithPascalString(nil, 'File', kCFStringEncodingUTF8);

  { Creates the file menu }

  Result := NSMenu.initWithTitle(MenuTitle);

  { Adds items to it }

  OpenItem := CreateMenuItem('Open');
  Result.addItem(OpenItem.Handle);
  SaveItem := CreateMenuItem('Save');
  Result.addItem(SaveItem.Handle);
  ExitItem := CreateMenuItem('Exit');
  Result.addItem(ExitItem.Handle);
end;

{@@
}
procedure TMyView.AddToMenubar(menu: NSMenu);
var
  dummyItem: NSMenuItem;
begin
  dummyItem := NSMenuItem.initWithTitle_action_keyEquivalent(CFEmptyString, nil, CFEmptyString);
  dummyItem.setSubmenu(menu.Handle);
  MainMenu.addItem(dummyItem.Handle);
  dummyItem.Free;
end;

{@@
}
function TMyView.CreateToolbar(AOwnerView: NSView; AX, AY, AWidth,
  AHeight: Double): NSToolbar;
begin
  Result := NSToolbar.initWithIdentifier(CFEmptyString);
  myToolbarController := TMyToolbarController.Create;
  Result.setDelegate(myToolbarController.Handle);
end;

{@@
}
procedure TMyView.CreateMainMenu();
begin
  MainMenu := NSMenu.initWithTitle(CFEmptyString);

  NSApp.setMainMenu(MainMenu.Handle);

  AppleMenu := CreateAppleMenu();
  NSApp.setAppleMenu(AppleMenu.Handle);
  AddToMenubar(AppleMenu);

  FileMenu := CreateFileMenu();
  AddToMenubar(FileMenu);
end;

{@@
}
function TMyView.CreateMenuItem(ATitle: shortstring): NSMenuItem;
var
  ItemText: CFStringRef;
  KeyText: CFStringRef;
begin
  KeyText := CFStringCreateWithPascalString(nil, '', kCFStringEncodingUTF8);
  ItemText := CFStringCreateWithPascalString(nil, ATitle, kCFStringEncodingUTF8);
  WriteLn(' ItemText: ', IntToHex(Int64(ItemText), 8), ' ATitle: ', ATitle);

  Result := NSMenuItem.initWithTitle_action_keyEquivalent(ItemText, nil, KeyText);
end;

{@@
  Creates an autosized NSTextField
}
function TMyView.CreateTextField(): NSTextField;
var
  ClientRect: NSRect;
begin
  ClientRect.origin.x := 0.0;
  ClientRect.origin.y := 0.0;
  ClientRect.size.width := 300.0;
  ClientRect.size.height := 500.0;

  Result := NSTextField.initWithFrame(ClientRect);
  Result.setAutoresizingMask(NSViewWidthSizable or NSViewHeightSizable);
  MainWindowView.addSubview(Result.Handle);
end;

end.


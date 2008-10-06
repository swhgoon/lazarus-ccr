{
 controller.pas

 Controller class for the texteditor example.

 This example project is released under public domain

 AUTHORS: Felipe Monteiro de Carvalho
}
unit controller;

{$mode delphi}{$STATIC ON}

interface

uses
  Classes, SysUtils, foundation, objc, appkit, MacOSAll;

type

  { TMyController }

  TMyController = class(NSObject)
  public
    { Extra binding functions }
    constructor Create; override;
    procedure AddMethods; override;
    { Objective-c Methods }
    class procedure doClose(_self: objc.id; _cmd: SEL; sender: objc.id); cdecl; {$ifndef VER2_2_2}static;{$endif}
    class procedure doOpenFile(_self: objc.id; _cmd: SEL; sender: objc.id); cdecl; {$ifndef VER2_2_2}static;{$endif}
    class procedure doSaveFile(_self: objc.id; _cmd: SEL; sender: objc.id); cdecl; {$ifndef VER2_2_2}static;{$endif}
    class procedure doSaveAsFile(_self: objc.id; _cmd: SEL; sender: objc.id); cdecl; {$ifndef VER2_2_2}static;{$endif}
    class function  applicationShouldTerminateAfterLastWindowClosed(_self: objc.id;
     _cmd: SEL; theApplication: objc.id): cbool; cdecl; {$ifndef VER2_2_2}static;{$endif}
  end;

const
  Str_doClose = 'doClose:';
  Str_doOpenFile = 'doOpenFile:';
  Str_doSaveFile = 'doSaveFile:';
  Str_doSaveAsFile = 'doSaveAsFile:';
  Str_applicationShouldTerminateAfterLastWindowClosed = 'applicationShouldTerminateAfterLastWindowClosed:';

var
  myController: TMyController;
  OpenPanel: NSOpenPanel;
  SavePanel: NSSavePanel;

implementation

uses view, model;

{ TMyController }

{@@
  Adds methods to the class

  Details of the parameters string:

  The first parameter is the result (v = void),
  followed by self and _cmd (@ = id and : = SEL),
  and on the end "sender" (@ = id)
}
procedure TMyController.AddMethods;
begin
  AddMethod(Str_doClose, 'v@:@', Pointer(doClose));
  AddMethod(Str_doOpenFile, 'v@:@', Pointer(doOpenFile));
  AddMethod(Str_doSaveFile, 'v@:@', Pointer(doSaveFile));
  AddMethod(Str_doSaveAsFile, 'v@:@', Pointer(doSaveAsFile));
  AddMethod(Str_applicationShouldTerminateAfterLastWindowClosed, 'B@:@',
   Pointer(applicationShouldTerminateAfterLastWindowClosed));
end;

constructor TMyController.Create;
begin
  { The class is registered on the Objective-C runtime before the NSObject constructor is called }
  if not CreateClassDefinition(ClassName(), Str_NSObject) then WriteLn('Failed to create objc class ' + ClassName());

  inherited Create;

  { Create objects }

  OpenPanel := NSOpenPanel.openPanel;
  SavePanel := NSSavePanel.savePanel;
end;

{ Objective-c Methods }

class procedure TMyController.doClose(_self: objc.id; _cmd: SEL; sender: objc.id); cdecl;
begin
  myView.MainWindow.close;
end;

class procedure TMyController.doOpenFile(_self: objc.id; _cmd: SEL;
  sender: objc.id); cdecl;
var
  FileContents: NSString;
begin
  { Show dialog }

  if OpenPanel.runModal = NSFileHandlingPanelOKButton then
  begin
    CFStringGetPascalString(OpenPanel.filename, @myModel.DocumentName, 255, kCFStringEncodingUTF8);

    { Now move the contents of the edit control to the file }

    FileContents := NSString.initWithContentsOfFile(OpenPanel.filename);

    myView.TextField.setStringValue(CFStringRef(FileContents.Handle));
  end;
end;

class procedure TMyController.doSaveFile(_self: objc.id; _cmd: SEL;
  sender: objc.id); cdecl;
var
  FileContents: NSString;
begin
  if myModel.DocumentName = Str_Untitled then doSaveAsFile(_self, _cmd, sender)
  else
  begin
    { Now move the contents of the file to the edit control }

    FileContents := NSString.CreateWithHandle(objc.id(myView.TextField.StringValue));

    FileContents.writeToFile_atomically(CFStringCreateWithPascalString(nil, myModel.DocumentName, kCFStringEncodingUTF8), False);
  end;
end;

class procedure TMyController.doSaveAsFile(_self: objc.id; _cmd: SEL;
  sender: objc.id); cdecl;
var
  FileContents: NSString;
begin
  { Show dialog }

  if SavePanel.runModal = NSFileHandlingPanelOKButton then
  begin
    CFStringGetPascalString(SavePanel.filename, @myModel.DocumentName, 255, kCFStringEncodingUTF8);

    { Now move the contents of the file to the edit control }

    FileContents := NSString.CreateWithHandle(objc.id(myView.TextField.StringValue));

    FileContents.writeToFile_atomically(CFStringCreateWithPascalString(nil, myModel.DocumentName, kCFStringEncodingUTF8), False);
  end;
end;

class function TMyController.applicationShouldTerminateAfterLastWindowClosed(_self: objc.id;
 _cmd: SEL; theApplication: objc.id): cbool; cdecl;
begin
  Result := objc.YES;
end;

end.


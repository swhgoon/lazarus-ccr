unit iOSIdeIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, ProjectIntf, iPhoneExtOptions;

type

  { TiOSApplicationDescriptor }

  TiOSApplicationDescriptor = class(TProjectDescriptor)
  public
    constructor Create; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function InitProject(AProject: TLazProject): TModalResult; override;
    function CreateStartFiles(AProject: TLazProject): TModalResult; override;
  end;

  { TiOSObjectDelegateWindowFileDesc }

  TiOSObjectDelegateWindowFileDesc = class(TFileDescPascalUnitWithResource)
  protected
    function GetDelegateProtocols: string; virtual;
  public
    constructor Create; override;
    function GetInterfaceUsesSection: string; override;
    function GetImplementationSource(const Filename, SourceName, ResourceName: string): string;override;
    function GetInterfaceSource(const Filename, SourceName, ResourceName: string): string; override;
    function GetUnitDirectives: string; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
  end;


  { TiOSAppDelegateWindowFileDesc }

  TiOSAppDelegateWindowFileDesc = class(TiOSObjectDelegateWindowFileDesc)
  protected
    function GetDelegateProtocols: string; override;
  public
    constructor Create; override;
  end;

  { TiOSInfo_PlistFileDesc }

  TiOSInfo_PlistFileDesc = class(TProjectFileDescriptor)
  public
    constructor Create; override;
    function CreateSource(const Filename, SourceName, ResourceName: string): string; override;
  end;

var
  GiOSApplicationDescriptor: TiOSApplicationDescriptor;
  GiOSAppDelegateWindowFileDesc: TiOSAppDelegateWindowFileDesc;
  GiOSInfo_PlistFileDesc: TiOSInfo_PlistFileDesc;
  GiOSObjectDelegateWindowFileDesc: TiOSObjectDelegateWindowFileDesc;

procedure register;

implementation

uses LazIDEIntf, Controls, iOS_Views, iOSXIBResource, UnitResources;

procedure register;
begin
  GiOSApplicationDescriptor:=TiOSApplicationDescriptor.Create;
  RegisterProjectDescriptor(GiOSApplicationDescriptor);
  GiOSAppDelegateWindowFileDesc:=TiOSAppDelegateWindowFileDesc.Create;
  RegisterProjectFileDescriptor(GiOSAppDelegateWindowFileDesc);
  GiOSInfo_PlistFileDesc:=TiOSInfo_PlistFileDesc.Create;
  RegisterProjectFileDescriptor(GiOSInfo_PlistFileDesc);
  GiOSObjectDelegateWindowFileDesc:=TiOSObjectDelegateWindowFileDesc.Create;
  RegisterProjectFileDescriptor(GiOSObjectDelegateWindowFileDesc);

  RegisterUnitResourcefileFormat(TXIBResourcefileFormat);
end;

{ TiOSObjectDelegateWindowFileDesc }

function TiOSObjectDelegateWindowFileDesc.GetDelegateProtocols: string;
begin
  result := '';
end;

constructor TiOSObjectDelegateWindowFileDesc.Create;
begin
  inherited Create;
  Name:='iOS NIB-Delegate';
  ResourceClass:=NSObject;
  DefaultResFileExt:='.xib';
  UseCreateFormStatements:=false;
  VisibleInNewDialog:=true;
end;

function TiOSObjectDelegateWindowFileDesc.GetInterfaceUsesSection: string;
begin
  Result:='iPhoneAll';
end;

function TiOSObjectDelegateWindowFileDesc.GetImplementationSource(
  const Filename, SourceName, ResourceName: string): string;
begin
  Result:='procedure T'+ResourceName+'.dealloc;' + LineEnding +
    'begin' + LineEnding +
    '  inherited dealloc;' + LineEnding +
    'end;' + LineEnding +
    LineEnding +
    '{$FakeResource *.xib}' + LineEnding + LineEnding;
end;

function TiOSObjectDelegateWindowFileDesc.GetInterfaceSource(const Filename, SourceName, ResourceName: string): string;
var
  DelegateProtocol: string;
begin
  DelegateProtocol:=GetDelegateProtocols;
  if DelegateProtocol<>'' then
    DelegateProtocol:=','+DelegateProtocol;
  Result:=
     'type'+LineEnding
    +'  T'+ResourceName+' = objcclass('+ResourceClass.ClassName+DelegateProtocol+')'+LineEnding
    +'  private'+LineEnding
    +'    { private declarations }'+LineEnding
    +'  public'+LineEnding
    +'    procedure dealloc; override;'+LineEnding
    +'  end;'+LineEnding
    +LineEnding;
end;

function TiOSObjectDelegateWindowFileDesc.GetUnitDirectives: string;
begin
  Result:='{$modeswitch ObjectiveC1}';
end;

function TiOSObjectDelegateWindowFileDesc.GetLocalizedName: string;
begin
  Result:='iOS NIB Delegate';
end;

function TiOSObjectDelegateWindowFileDesc.GetLocalizedDescription: string;
begin
  Result:='Create a new iOS-NIB file with a delegate to handle it''s contents. '+
    'The contents of the NIB can be changed with the designer.';
end;

{ TiOSInfo_PlistFileDesc }

constructor TiOSInfo_PlistFileDesc.Create;
begin
  inherited Create;
  Name:='Info_Plist';
  DefaultFilename:='Info.Plist';
  IsPascalUnit:=false;
  VisibleInNewDialog:=false;
end;

function TiOSInfo_PlistFileDesc.CreateSource(const Filename, SourceName, ResourceName: string): string;
begin
  Result:='<?xml version="1.0" encoding="UTF-8"?>' + LineEnding +
    '<!DOCTYPE plist public "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">' + LineEnding +
    '<plist version="1.0">' + LineEnding +
    '<dict>' + LineEnding +
    '	<key>CFBundleDevelopmentRegion</key>' + LineEnding +
    '	<string>en</string>' + LineEnding +
    '	<key>CFBundleDisplayName</key>' + LineEnding +
    '	<string>${PRODUCT_NAME}</string>' + LineEnding +
    '	<key>CFBundleExecutable</key>' + LineEnding +
    '	<string>${EXECUTABLE_NAME}</string>' + LineEnding +
    '	<key>CFBundleIconFile</key>' + LineEnding +
    '	<string></string>' + LineEnding +
    '	<key>CFBundleIdentifier</key>' + LineEnding +
    '	<string>CNOC.${PRODUCT_NAME:rfc1034identifier}</string>' + LineEnding +
    '	<key>CFBundleInfoDictionaryVersion</key>' + LineEnding +
    '	<string>6.0</string>' + LineEnding +
    '	<key>CFBundleName</key>' + LineEnding +
    '	<string>${PRODUCT_NAME}</string>' + LineEnding +
    '	<key>CFBundlePackageType</key>' + LineEnding +
    '	<string>APPL</string>' + LineEnding +
    '	<key>CFBundleShortVersionString</key>' + LineEnding +
    '	<string>1.0</string>' + LineEnding +
    '	<key>CFBundleSignature</key>' + LineEnding +
    '	<string>????</string>' + LineEnding +
    '	<key>CFBundleVersion</key>' + LineEnding +
    '	<string>1.0</string>' + LineEnding +
    '	<key>LSRequiresIPhoneOS</key>' + LineEnding +
    '	<true/>' + LineEnding +
    '	<key>NSMainNibFile</key>' + LineEnding +
    '	<string>MainWindow_iPhone</string>' + LineEnding +
    '	<key>NSMainNibFile~ipad</key>' + LineEnding +
    '	<string>MainWindow_iPad</string>' + LineEnding +
    '	<key>UISupportedInterfaceOrientations</key>' + LineEnding +
    '	<array>' + LineEnding +
    '		<string>UIInterfaceOrientationLandscapeLeft</string>' + LineEnding +
    '		<string>UIInterfaceOrientationLandscapeRight</string>' + LineEnding +
    '		<string>UIInterfaceOrientationPortrait</string>' + LineEnding +
    '	</array>' + LineEnding +
    '	<key>UISupportedInterfaceOrientations~ipad</key>' + LineEnding +
    '	<array>' + LineEnding +
    '		<string>UIInterfaceOrientationPortrait</string>' + LineEnding +
    '		<string>UIInterfaceOrientationPortraitUpsideDown</string>' + LineEnding +
    '		<string>UIInterfaceOrientationLandscapeLeft</string>' + LineEnding +
    '		<string>UIInterfaceOrientationLandscapeRight</string>' + LineEnding +
    '	</array>' + LineEnding +
    '</dict>' + LineEnding +
    '</plist>'+LineEnding+LineEnding;
end;

{ TiOSAppDelegateWindowFileDesc }

function TiOSAppDelegateWindowFileDesc.GetDelegateProtocols: string;
begin
  Result:='UIApplicationDelegateProtocol';
end;

constructor TiOSAppDelegateWindowFileDesc.Create;
begin
  inherited Create;
  Name:='iOS UIApplicationDelegate';
  VisibleInNewDialog:=false;
end;

{ TiOSApplicationDescriptor }

constructor TiOSApplicationDescriptor.Create;
begin
  inherited Create;
  Name := 'iOS application';
end;

function TiOSApplicationDescriptor.GetLocalizedName: string;
begin
  Result := 'iOS application';
end;

function TiOSApplicationDescriptor.GetLocalizedDescription: string;
begin
  Result:='iOS application'#13#13'An iOS program '
          +'designed in Lazarus without using the LCL. The program file is '
          +'automatically maintained by Lazarus.';
end;

function TiOSApplicationDescriptor.InitProject(AProject: TLazProject): TModalResult;
var
  NewSource: String;
  MainFile: TLazProjectFile;
begin
  inherited InitProject(AProject);

  MainFile:=AProject.CreateProjectFile('iosapp.lpr');
  MainFile.IsPartOfProject:=true;
  AProject.AddFile(MainFile,false);
  AProject.MainFileID:=0;

  // create program source
  NewSource:='program iosapp;' + LineEnding +
    LineEnding +
    '{$modeswitch ObjectiveC1}' + LineEnding +
    LineEnding +
    'uses' + LineEnding +
    '  iPhoneAll, AppDelegate_iPhoneU;' + LineEnding +
    LineEnding +
    'var' + LineEnding +
    '  pool : NSAutoreleasePool;' + LineEnding +
    'begin' + LineEnding +
    '  pool := NSAutoreleasePool.alloc.init;' + LineEnding +
    '  UIApplicationMain(argc, argv, nil, nil);' + LineEnding +
    '  pool.release;' + LineEnding +
    'end.' + LineEnding + LineEnding;

  AProject.MainFile.SetSourceText(NewSource);

  // add
  AProject.AddPackageDependency('FCL');

  // compiler options
  AProject.LazCompilerOptions.Win32GraphicApp:=false;

  //AProject.LazCompilerOptions.CustomOptions:='-XR/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneSimulator.platform/Developer/SDKs/iPhoneSimulator5.1.sdk';
  AProject.LazCompilerOptions.TargetOS:='iphonesim';
  AProject.Flags := AProject.Flags - [pfMainUnitHasCreateFormStatements];
  Result:= mrOK;
end;

function TiOSApplicationDescriptor.CreateStartFiles(AProject: TLazProject): TModalResult;
begin
  //LazarusIDE.DoSaveAll([sfProjectSaving]);
  GiOSAppDelegateWindowFileDesc.DefaultResourceName:='AppDelegate_iPhone';
  LazarusIDE.DoNewEditorFile(GiOSAppDelegateWindowFileDesc,'appdelegate_iphoneu.pas','',[nfIsPartOfProject,nfOpenInEditor,nfCreateDefaultSrc]);
  result := mrOK;
  ProjOptions.isIPhoneApp:=true;
  ProjOptions.MainNib:='appdelegate_iphoneu';
  ProjOptions.Save;
end;

end.


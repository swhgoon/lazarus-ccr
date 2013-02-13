unit iOSIdeIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, ProjectIntf, iPhoneExtOptions, ComponentEditors;

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

  { TiOSShowInXCode }

  TiOSShowInXCode = Class(TComponentEditor)
  private
    FStartIndex : Integer;
  Public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;


var
  GiOSApplicationDescriptor: TiOSApplicationDescriptor;
  GiOSAppDelegateWindowFileDesc: TiOSAppDelegateWindowFileDesc;
  GiOSObjectDelegateWindowFileDesc: TiOSObjectDelegateWindowFileDesc;

resourcestring
  SShowInXCode = 'Show in XCode';

procedure register;

implementation

uses
  LazIDEIntf, Controls, iOS_Views, iOSXIBResource, UnitResources,
  LazFilesUtils,
  sysutils,
  FileUtil;

procedure register;
begin
  GiOSApplicationDescriptor:=TiOSApplicationDescriptor.Create;
  RegisterProjectDescriptor(GiOSApplicationDescriptor);
  GiOSAppDelegateWindowFileDesc:=TiOSAppDelegateWindowFileDesc.Create;
  RegisterProjectFileDescriptor(GiOSAppDelegateWindowFileDesc);
  GiOSObjectDelegateWindowFileDesc:=TiOSObjectDelegateWindowFileDesc.Create;
  RegisterProjectFileDescriptor(GiOSObjectDelegateWindowFileDesc);

  RegisterComponentEditor(NSObject, TiOSShowInXCode);

  RegisterUnitResourcefileFormat(TXIBResourcefileFormat);
end;

{ TiOSShowInXCode }

procedure TiOSShowInXCode.ExecuteVerb(Index: Integer);
var
  s: string;
  ProjFile: TLazProjectFile;
begin
  If Index<FStartIndex then
    inherited ExecuteVerb(Index)
  else
    case (Index-FstartIndex) of
      0 : begin
          ProjFile := LazarusIDE.GetProjectFileWithRootComponent(Component);
          s := ChangeFileExt(ProjFile.Filename,'.xib');
          if FileExistsUTF8(s) then ExecCmdLineNoWait('open "'+s+'"');
          end;
    end;
end;

function TiOSShowInXCode.GetVerb(Index: Integer): string;
begin
  If Index<FStartIndex then
    Result:=inherited GetVerb(Index)
  else
    case (Index-FstartIndex) of
      0 : Result:=SShowInXCode;
    end;
end;

function TiOSShowInXCode.GetVerbCount: Integer;
begin
  FStartIndex:=inherited GetVerbCount;
  Result:=FStartIndex+1;
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

  ProjOptions.Reset;
  ProjOptions.isIPhoneApp:=true;
  ProjOptions.MainNib:='appdelegate_iphoneu';
  ProjOptions.Save;
end;

end.


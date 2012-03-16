unit GeckoDirectoryService;

{$MACRO on}

{$IFDEF Windows}
  {$DEFINE extdecl:=stdcall}
{$ELSE Windows}
  {$DEFINE extdecl:=cdecl}
{$ENDIF}

interface

uses
  SysUtils, Classes, nsXPCOM;

type
  TGeckoDirectoryServiceOnGetFile = function (
    const AProp: AnsiString): String of object;
  TGeckoDirectoryServiceOnGetFileList = function (
    const AProp: AnsiString; AList: TStrings): Boolean of Object;

  TCustomDirectoryServiceProvider = class(TComponent)
  private
    { Private declarations }
    FOnGetFile: TGeckoDirectoryServiceOnGetFile;
    FOnGetFileList: TGeckoDirectoryServiceOnGetFileList;
    FProxy: nsIDirectoryServiceProvider;

    function ProcessGetFile(const AProp: AnsiString; var APersistent: Boolean)
      : String;
    function ProcessGetFileList(const AProp: AnsiString;
                                AFileList: TStrings): Boolean;
    property OnGetFile: TGeckoDirectoryServiceOnGetFile read FOnGetFile
      write FOnGetFile;
    property OnGetFileList: TGeckoDirectoryServiceOnGetFileList
      read FOnGetFileList write FOnGetFileList;
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
  end;

  TGeckoDirectoryServiceProvider = class(TCustomDirectoryServiceProvider)
  private
    { Private declarations }
    FRegDir: AnsiString;
    FRegFile: AnsiString;
    FDefaultsDir: AnsiString;
    FPrefDefaultsDir: AnsiString;
    FProfileDefaultsDir: AnsiString;
    FProfileDefaultsNlocDir: AnsiString;

  protected
    { Protected declarations }
  public
    { Public declarations }
  published
    { Published declarations }
    property OnGetFile;
  end;

procedure Register;

implementation

uses
  nsTypes, nsInit, nsError, nsGeckoStrings, nsXPCOMGlue, nsCID, GeckoInit,
  nsXPCOM_std19, nsEnumerators, Windows;

type
  TDirectoryServiceProviderProxy = class(TInterfacedObject,
                                         nsIDirectoryServiceProvider_std19,
                                         nsIDirectoryServiceProvider2_std19)
    FOwner: TCustomDirectoryServiceProvider;
    constructor Create(AOwner: TCustomDirectoryServiceProvider);
    function GetFile(const prop: PAnsiChar; out persistent: PRBool;
      out _retval: nsIFile_std19): nsresult; extdecl;
    function GetFiles(const prop: PAnsiChar;
      out _retval: nsISimpleEnumerator_std19): nsresult; extdecl;
  end;

procedure Register;
begin
  RegisterComponents('Gecko', [TGeckoDirectoryServiceProvider]);
end;

constructor TDirectoryServiceProviderProxy.Create(
  AOwner: TCustomDirectoryServiceProvider);
begin
  inherited Create;
  FOwner := AOwner;
end;

var
  sModulePath: array[0..MAX_PATH] of Char;
  sModulePathInitialized: Boolean = False;

function GetModulePath: String;
begin
  if not sModulePathInitialized then
  begin
    GetModuleFileName(0, sModulePath, MAX_PATH);
    sModulePathInitialized := True;
  end;
  Result := PChar(@sModulePath);
end;

function MakeILocalFile(const AFilename: String;
  out ALocalFile: nsILocalFile): nsresult;
var
  localFile: nsILocalFile;
{$ifndef UNICODE}
  localFileStr: IInterfacedCString;
{$else}
  localFileStr: IInterfacedString;
{$endif}
  modulePath: String;
  targetPath, relativePath: String;
begin
  modulePath := GetModulePath;

  if (Length(AFilename)>0) and
     (AFilename[1] = '\') then
  begin
    // case of Path from root or network path
    if (Length(afilename)>=2) and
       (afilename[2] <> '\') then
    begin
      // case of Path from root
      targetPath := ExtractFileDrive(modulePath) + afilename;
    end else
    begin
      // case of network path
      targetPath := afilename;
    end;
  end else
  if (Length(afilename)<3) or
     (afilename[2] <> ':') then
  begin
    // case of relative path
    targetPath := ExtractFileDir(modulePath);
    if Length(afilename)>0 then
      relativePath := afilename;
  end else
  begin
    // case of full path
    targetPath := afilename;
  end;

  try
{$ifndef UNICODE}
    localFileStr := NewCString(targetPath);
{$else}
    localFileStr := NewString(targetPath);
{$endif}
  except
    Result := NS_ERROR_FAILURE;
    Exit;
  end;
{$ifndef UNICODE}
  Result := NS_NewNativeLocalFile(localFileStr.ACString, True, localFile);
{$else}
  Result := NS_NewLocalFile(localFileStr.AString, True, localFile);
{$endif}
  if NS_FAILED(Result) then Exit;

  if Length(relativePath)>0 then
  begin
    localFileStr.Assign(relativePath);
{$ifndef UNICODE}
    localFile.AppendRelativeNativePath(localFileStr.ACString);
{$else}
    localFile.AppendRelativePath(localFileStr.AString);
{$endif}
  end;

  Result := localFile.QueryInterface(nsILocalFile, ALocalFile);
end;

function TDirectoryServiceProviderProxy.GetFile(const prop: PAnsiChar;
  out persistent: PRBool; out _retval: nsIFile_std19): nsresult;
var
  path: String;
  localFile: nsILocalFile;
  persist: Boolean;
begin
  if Assigned(FOwner) then
  begin
    path := FOwner.ProcessGetFile(prop, persist);
    if (Length(path)=0) and Assigned(FOwner.FOnGetFile) then
    begin
      path := FOwner.FOnGetFile(prop);
    end;
  end;

  if Length(path)=0 then
  begin
    Result := NS_ERROR_FAILURE;
  end else
  begin
    Result := MakeILocalFile(path, localFile);
    if NS_SUCCEEDED(Result) then
    begin
      Result := localFile.QueryInterface(nsIFile_std19, _retval);
    end;
  end;
end;

function TDirectoryServiceProviderProxy.GetFiles(const prop: PAnsiChar;
  out _retval: nsISimpleEnumerator_std19): nsresult;
var
  enum: nsISimpleEnumerator;
  lfile: nsILocalFile;
  done: Boolean;
  list: TStringList;
  intfList: TInterfaceList;
  i: Integer;
begin
  list := TStringList.Create;
  try
    intfList := TInterfaceList.Create;
    try
      enum := NewSimpleEnumeratorFromTInterfaceList(intfList, True);
    except
      Result := NS_ERROR_FAILURE;
      intfList.Free;
      Exit;
    end;

    if Assigned(FOwner) then
    begin
      Done := FOwner.ProcessGetFileList(prop, list);
      if not Done then
      begin
        Result := NS_ERROR_FAILURE;
        Exit;
      end;
      for i:=0 to list.Count -1 do
      begin
        Result := MakeILocalFile(list[i], lfile);
        if NS_FAILED(Result) then
          Exit;
        intfList.Add(lfile);
      end;
      Result := enum.QueryInterface(nsISimpleEnumerator, _retval);
    end else
    begin
      Result := NS_ERROR_FAILURE;
    end;
  finally
    list.Free;
  end;
end;

constructor TCustomDirectoryServiceProvider.Create(AOwner: TComponent);
var
  dirSrv: nsIDirectoryService;
begin
  inherited;

  if not (csDesigning in ComponentState) then
  begin
    GeckoComponentsStartup;

    FProxy := TDirectoryServiceProviderProxy.Create(Self)
      as nsIDirectoryServiceProvider;
    NS_GetService(NS_DIRECTORY_SERVICE_CONTRACTID, nsIDirectoryService, dirSrv);
    dirSrv.RegisterProvider(FProxy);
  end;
end;

destructor TCustomDirectoryServiceProvider.Destroy;
var
  dirSrv: nsIDirectoryService;
begin
  if not (csDesigning in ComponentState) then
  begin
    NS_GetService(NS_DIRECTORY_SERVICE_CONTRACTID, nsIDirectoryService, dirSrv);
    dirSrv.UnregisterProvider(FProxy);

    GeckoComponentsShutdown;
  end;

  inherited;
end;

function TCustomDirectoryServiceProvider.ProcessGetFile(const AProp: AnsiString;
  var APersistent: Boolean): String;
begin
end;

function TCustomDirectoryServiceProvider.ProcessGetFileList(
  const AProp: AnsiString; AFileList: TStrings): Boolean;
begin
  Result := False;
end;

end.

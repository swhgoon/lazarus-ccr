unit nsXRE;

interface

uses
  Classes, nsTypes, nsXPCOM, nsInit;

type
  PXREAppData = ^nsXREAppData;
  nsXREAppData = record
    size: PRUint32;
    directory: nsILocalFile;
    vendor: PAnsiChar;
    name: PAnsiChar;
    version: PAnsiChar;
    buildID: PAnsiChar;
    ID: PAnsiChar;
    copyright: PAnsiChar;
    flags: PRUint32;
    xreDirectory: nsILocalFile;
    minVersion: PAnsiChar;
    maxVersion: PAnsiChar;
    crashReportURL: PAnsiChar;
    profile: PAnsiChar;
  end;

const
  NS_XRE_ENABLE_PROFILE_MIGRATOR  = 1 shl 1;
  NS_XRE_ENABLE_EXTENSION_MANEGER = 1 shl 2;
  NS_XRE_ENABLE_CRASH_REPORTER    = 1 shl 3;

  XRE_USER_APP_DATA_DIR              = 'UAppData';
  XRE_EXTENSIONS_DIR_LIST            = 'XREExtDL';
  XRE_EXECUTABLE_FILE                = 'XREExeF';
  NS_APP_PROFILE_DIR_STARTUP         = 'ProfDS';
  NS_APP_PROFILE_LOCAL_DIR_STARTUP   = 'ProfLDS';
  XRE_SYS_LOCAL_EXTENSION_PARENT_DIR = 'XRESysLExtPD';
  XRE_SYS_SHARE_EXTENSION_PARENT_DIR = 'XRESysSExtPD';
  XRE_USER_SYS_EXTENSION_DIR         = 'XREUSysExt';

{$IFNDEF MSWINDOWS}
  MAX_PATH = 260;
{$ENDIF}

// XRE Functions
function XRE_FindGRE(const lowerVer: PAnsiChar;
                     lowerInclusive: PRBool;
                     const upperVer: PAnsiChar;
                     upperInclusive: PRBool;
                     GREPath: PAnsiChar;
                     GREPathLen: PRUint32): nsresult;
function XRE_LoadGRE(GREPath: PAnsiChar): nsresult;
function XRE_UnloadGRE(): nsresult;
function XRE_FindAndLoadGRE(const lowerVer: PAnsiChar;
                     lowerInclusive: PRBool;
                     const upperVer: PAnsiChar;
                     upperInclusive: PRBool;
                     XPComPath: string = ''): nsresult;
function XRE_GetLoadedGREPath(GREPath: PAnsiChar;
                              GREPathLen: Cardinal): nsresult;

function XRE_Startup(const lowerVer: PAnsiChar;
                     lowerInclusive: PRBool;
                     const upperVer: PAnsiChar;
                     upperInclusive: PRBool;
                     XPComPath: string = ''): nsresult;
function XRE_Shutdown(): nsresult;

function XRE_main(argc: integer;
                  argv: Pointer;
                  const sAppData: nsXREAppData): Integer; cdecl;

function XRE_GetFileFromPath(aPath: PAnsiChar;
                             out aResult: nsILocalFile): nsresult; cdecl;
function XRE_GetBinaryPath(argv0: PAnsiChar;
                           out aResult: nsILocalFile): nsresult; cdecl;
procedure XRE_GetStaticComponents(out aStaticComponents: PStaticModuleInfoArray;
                                  out aComponentCount: PRUint32); cdecl;
function XRE_LockProfileDirectory(aDirectory: nsILocalFile;
                                  out aLockObject: nsISupports): nsresult; cdecl;
function XRE_InitEmbedding(aLibXulDirectory: nsILocalFile;
                           aAppDirectory: nsILocalFile;
                           aAppDirProvider: nsIDirectoryServiceProvider;
                           const aStaticComponents: PStaticModuleInfoArray;
                           aStaticComponentCount: PRUint32): nsresult;
procedure XRE_NotifyProfile(); cdecl;
procedure XRE_TermEmbedding(); cdecl;
function XRE_CreateAppData(aINIFile: nsILocalFile;
                           out aAppData: PXREAppData): nsresult; cdecl;
function XRE_ParseAppData(aINIFile: nsILocalFile;
                          out aAppData: nsXREAppData): nsresult; cdecl;
procedure XRE_FreeAppData(aAppData: PXREAppData); cdecl;

implementation

uses
  nsError, nsGeckoStrings,
  {$IFDEF MSWINDOWS} Windows, {$ELSE} DynLibs, {$ENDIF} SysUtils;

var
  mainFunc :
    function (argc: integer;
              argv: Pointer;
              const sAppData: nsXREAppData): Integer; cdecl;
  getFileFromPathFunc :
    function (aPath: PAnsiChar;
              out aResult: nsILocalFile): nsresult; cdecl;
  getBinaryPathFunc :
    function (argv0: PAnsiChar;
              out aResult: nsILocalFile): nsresult; cdecl;
  getStaticComponentsFunc :
    procedure (out aStaticComponents: PStaticModuleInfoArray;
              out aComponentCount: PRUint32); cdecl;
  lockProfileDirectoryFunc :
    function (aDirectory: nsILocalFile;
              out aLockObject: nsISupports): nsresult; cdecl;
  initEmbeddingFunc :
    function (aLibXulDirectory: nsILocalFile;
              aAppDirectory: nsILocalFile;
              aAppDirProvider: nsIDirectoryServiceProvider;
              const aStaticComponents: PStaticModuleInfoArray;
              aStaticComponentCount: PRUint32): nsresult; cdecl;
  {$IFDEF XULRUNNER2}
  initEmbedding2Func :
    function (aLibXulDirectory: nsILocalFile;
              aAppDirectory: nsILocalFile;
              aAppDirProvider: nsIDirectoryServiceProvider): nsresult; cdecl;
  {$ENDIF}
  notifyProfileFunc : procedure (); cdecl;
  termEmbeddingFunc : procedure (); cdecl;
  createAppDataFunc :
    function (aINIFile: nsILocalFile;
              out aAppData: PXREAppData): nsresult; cdecl;
  parseAppDataFunc :
    function (aINIFile: nsILocalFile;
              out aAppData: nsXREAppData): nsresult; cdecl;
  freeAppDataFunc :
    procedure (aAppData: PXREAppData); cdecl;

function strrpbrk(src: PAnsiChar; const charSet: PAnsiChar): PAnsiChar;
var
  ptr: PAnsiChar;
begin
  Result := nil;
  while src^ <> #0 do
  begin
    ptr := charSet;
    while ptr^ <> #0 do
    begin
      if ptr^ = src^ then
        Result := src;
      Inc(ptr);
    end;
    Inc(src);
  end;
end;

function XRE_FindGRE(const lowerVer: PAnsiChar;
                     lowerInclusive: PRBool;
                     const upperVer: PAnsiChar;
                     upperInclusive: PRBool;
                     GREPath: PAnsiChar;
                     GREPathLen: PRUint32): nsresult;
const
{$IFDEF MSWINDOWS}
  libxul = 'xul.dll';
{$ELSE}
  libxul = 'XUL';
{$ENDIF}
var
  vers: TGREVersionRange;
  xpcomPath: array[0..MAX_PATH] of AnsiChar;
  lastSlash: PAnsiChar;
  MOZILLAFIVEHOME: string;
  {$IFDEF LINUX}
  EachPath: string;
  LDPATH: string;
  LDPATHItems: TStringList;
  j: integer;
  {$ENDIF}
begin
{$IFDEF MSWINDOWS}
  vers.lower := lowerVer;
  vers.lowerInclusive := lowerInclusive;
  vers.upper := upperVer;
  vers.upperInclusive := upperInclusive;

(*
  Result := GRE_GetGREPathWithProperties(@vers, 1, nil, 0, xpcomPath, MAX_PATH);
//FPC port: previous call doesn't find Firefox's GRE, so just force it.
  if NS_FAILED(result) then
    begin
//    NS_StrLCopy(xpcomPath, 'C:\Program Files\Mozilla Firefox\xpcom.dll', MAX_PATH);
    NS_StrLCopy(xpcomPath, PChar(ExtractFilePath(ParamStr(0)) + 'xulrunner\xpcom.dll'), MAX_PATH);
    if FileExists(xpcomPath) then
      Result := NS_OK;
    end;
//FPC port
  if NS_FAILED(result) then
    Exit;
*)
  //Changed checking order. Preference is xulrunner in application folder
  MOZILLAFIVEHOME:=GetEnvironmentVariable('MOZILLA_FIVE_HOME');
  if MOZILLAFIVEHOME<>'' then begin
    if MOZILLAFIVEHOME[Length(MOZILLAFIVEHOME)]<>PathDelim then MOZILLAFIVEHOME:=MOZILLAFIVEHOME+PathDelim;
    NS_StrLCopy(xpcomPath, PChar(MOZILLAFIVEHOME+'xpcom.dll'), MAX_PATH);
    Result:=NS_OK;
  end else begin
    NS_StrLCopy(xpcomPath, PChar(ExtractFilePath(ParamStr(0)) + 'xpcom.dll'), MAX_PATH);
    if FileExists(xpcomPath) then begin
      Result := NS_OK;
    end else begin
      NS_StrLCopy(xpcomPath, PChar(ExtractFilePath(ParamStr(0)) + 'xulrunner\xpcom.dll'), MAX_PATH);
      if FileExists(xpcomPath) then begin
        Result := NS_OK;
      end else begin
        Result := GRE_GetGREPathWithProperties(@vers, 1, nil, 0, xpcomPath, MAX_PATH);
        if not FileExists(xpcomPath) then begin
          Result:=NS_ERROR_FILE_ACCESS_DENIED
        end else begin
          result:=NS_OK;
        end;
      end;
    end;
  end;
  if NS_FAILED(result) then
    Exit;

  lastSlash := strrpbrk(xpcomPath, '/\');
  if not Assigned(lastSlash) then
  begin
    Result := NS_ERROR_FILE_INVALID_PATH;
    Exit;
  end;

  NS_StrLCopy(GREPath, xpcomPath, GREPathLen);

  Exit;
{$ELSE}
 {$IFDEF DARWIN}
//  NS_StrLCopy(GREPath, '/Applications/Firefox.app/Contents/MacOS/libxpcom.dylib', GREPathLen);
  NS_StrLCopy(GREPath, '/Library/Frameworks/XUL.framework/Versions/Current/libxpcom.dylib', GREPathLen);
 {$ELSE}  //Linux
  //NS_StrLCopy(GREPath, '/home/user/xulrunner/libxpcom.so', GREPathLen);
  Result:=NS_ERROR_NOT_AVAILABLE;
  LDPATH:=GetEnvironmentVariable('LD_LIBRARY_PATH');
  LDPATHItems:=TStringList.Create;
  LDPATHItems.StrictDelimiter:=true;
  LDPATHItems.Delimiter:=PathSeparator;
  LDPATHItems.DelimitedText:=LDPATH;
  for j := 0 to LDPATHItems.Count-1 do begin
    EachPath:=LDPATHItems[j];
    if Length(EachPath)>0 then begin
      if EachPath[Length(EachPath)]<>PathDelim then EachPath:=EachPath+PathDelim;
      if FileExists(EachPath+'libxpcom.so') and FileExists(EachPath+'dependentlibs.list') then begin
        NS_StrLCopy(GREPath, pchar(EachPath+'libxpcom.so'), GREPathLen);
        Result:=NS_OK;
        break;
      end;
    end;
  end;
  LDPATHItems.Free;
 {$ENDIF}
  Result := NS_OK;
{$ENDIF}
end;

var
  sXulModule: HMODULE;
  sLoadedGREPath: array[0..MAX_PATH] of AnsiChar;

function XRE_LoadGRE(GREPath: PAnsiChar): nsresult;
const
{$IFDEF MSWINDOWS}
  libxul = 'xul.dll';
{$ELSE}
 {$IFDEF DARWIN}
  libxul = 'XUL';
 {$ELSE}  //Linux
  libxul = 'libxul.so';
 {$ENDIF}
{$ENDIF}
var
  xpcomPath, xulPath: array[0..MAX_PATH] of AnsiChar;
  xulModule: HMODULE;
  lastSlash: PAnsiChar;
begin
  if sXulModule <> 0 then
  begin
    Result := NS_ERROR_ALREADY_INITIALIZED;
    Exit;
  end;

  NS_StrLCopy(xpcomPath, GREPath, MAX_PATH);
  lastSlash := strrpbrk(xpcomPath, '/\');
  if not Assigned(lastSlash) then
  begin
      Result := NS_ERROR_FILE_INVALID_PATH;
      Exit;
  end;
  lastSlash^ := #0;
  NS_StrLCopy(xulPath, xpcomPath, MAX_PATH);
  NS_StrLCat(xulPath, DirectorySeparator + libxul, MAX_PATH);

  Result := XPCOMGlueStartup(GREPath);
  if NS_FAILED(Result) then
    Exit;

{$IFDEF MSWINDOWS}
  xulModule := LoadLibraryExA(xulPath, 0, 0);
{$ELSE}
  xulModule := LoadLibrary(xulPath);
{$ENDIF}
  if xulModule = 0 then
  begin
    Result := NS_ERROR_FAILURE;
    XPCOMGlueShutdown();
    Exit;
  end;

  mainFunc := GetProcAddress(xulModule, 'XRE_main');
  getFileFromPathFunc := GetProcAddress(xulModule, 'XRE_GetFileFromPath');
  getBinaryPathFunc := GetProcAddress(xulModule, 'XRE_GetBinaryPath');
  getStaticComponentsFunc := GetProcAddress(xulModule, 'XRE_GetStaticComponents');
  lockProfileDirectoryFunc := GetProcAddress(xulModule, 'XRE_LockProfileDirectory');
  initEmbeddingFunc := GetProcAddress(xulModule, 'XRE_InitEmbedding');
  {$IFDEF XULRUNNER2}
  initEmbedding2Func := GetProcAddress(xulModule, 'XRE_InitEmbedding2');
  {$ENDIF}
  notifyProfileFunc := GetProcAddress(xulModule, 'XRE_NotifyProfile');
  termEmbeddingFunc := GetProcAddress(xulModule, 'XRE_TermEmbedding');
  createAppDataFunc := GetProcAddress(xulModule, 'XRE_CreateAppData');
  parseAppDataFunc := GetProcAddress(xulModule, 'XRE_ParseAppData');
  freeAppDataFunc := GetProcAddress(xulModule, 'XRE_FreeAppData');

  NS_StrLCopy(sLoadedGREPath, GREPath, MAX_PATH);
  sXulModule := xulModule;
end;

function XRE_UnloadGRE(): nsresult;
begin
  if sXulModule = 0 then
  begin
    Result := NS_ERROR_NOT_INITIALIZED;
    Exit;
  end;
  XPCOMGlueShutdown;
  sXulModule := 0;
  Result := NS_OK;
end;

function XRE_FindAndLoadGRE(const lowerVer: PAnsiChar;
                     lowerInclusive: PRBool;
                     const upperVer: PAnsiChar;
                     upperInclusive: PRBool;
                     XPComPath: string = ''): nsresult;
var
  grePath: array[0..MAX_PATH] of AnsiChar;
begin
  if XPComPath='' then
    begin
    Result := XRE_FindGRE(lowerVer, lowerInclusive,
                          upperVer, upperInclusive,
                          grePath, MAX_PATH);
    if NS_FAILED(Result) then
      Exit;
    end
  else if FileExists(XPComPath) then
    begin
    NS_StrLCopy(GREPath, pchar(XPComPath), length(XPComPath));
    end
  else
    begin
    result := NS_ERROR_FILE_ACCESS_DENIED;
    Exit;
    end;

  Result := XRE_LoadGRE(grePath);
end;

function XRE_GetLoadedGREPath(GREPath: PAnsiChar;
                              GREPathLen: Cardinal): nsresult;
begin
  if sXulModule = 0 then
  begin
    Result := NS_ERROR_NOT_INITIALIZED;
    Exit;
  end;
  NS_StrLCopy(GREPath, sLoadedGREPath, GREPathLen);
  Result := NS_OK;
end;

function XRE_Startup(const lowerVer: PAnsiChar;
                     lowerInclusive: PRBool;
                     const upperVer: PAnsiChar;
                     upperInclusive: PRBool;
                     XPComPath: string = ''): nsresult;
var
  grePath: array[0..MAX_PATH] of AnsiChar;
  xulDir: nsILocalFile;
  lastSlash: PAnsiChar;

  appPath: array[0..MAX_PATH] of AnsiChar;
  appDir: nsILocalFile;
begin
  Result := XRE_FindAndLoadGRE(lowerVer, lowerInclusive,
                               upperVer, upperInclusive,
                               XPComPath);
  if NS_FAILED(Result) then
    Exit;

  XRE_GetLoadedGREPath(grePath, MAX_PATH);
  lastSlash := strrpbrk(grePath, '/\');
  lastSlash^ := #0;
{$IFNDEF FPC}
  Result :=  NS_NewNativeLocalFile(NewCString(grePath).ACString, False, xulDir);
{$ELSE}
  Result :=  NS_NewNativeLocalFile(NewCString(AnsiString(grePath)).ACString, False, xulDir);
{$ENDIF}
  if NS_FAILED(Result) then
  begin
    XRE_UnloadGRE();
    Exit;
  end;
{$IFDEF MSWINDOWS}
  if not NS_CurrentProcessDirectory(appPath, MAX_PATH) then
  begin
    Result := NS_ERROR_FAILURE;
    XRE_UnloadGRE();
    Exit;
  end;
{$ELSE}
//  NS_StrCopy(appPath, PAnsiChar(ExtractFilePath(ParamStr(0))));
  NS_StrCopy(appPath, PAnsiChar(ExtractFilePath(ExpandFileName(ParamStr(0)))));
{$ENDIF}
{$IFNDEF FPC}
  Result := NS_NewNativeLocalFile(NewCString(appPath).ACString, False, appDir);
{$ELSE}
  Result := NS_NewNativeLocalFile(NewCString(AnsiString(appPath)).ACString, False, appDir);
{$ENDIF}
  if NS_FAILED(Result) then
  begin
    XRE_UnloadGRE();
    Exit;
  end;
//  NS_LogInit();
  //Warning, do not pass GeckoEngineDirectoryService to XRE_InitEmbedding, it
  //will crash Gecko versions prior to 1.9.2.x with AV in line "basewin.Create"
  Result := XRE_InitEmbedding(xulDir, appDir, nil, nil, 0);
//  NS_LogTerm();
end;

function XRE_Shutdown(): nsresult;
begin
  XRE_TermEmbedding();
  XRE_UnloadGRE();
  Result := NS_OK;
end;

function XRE_main(argc: integer;
                  argv: Pointer;
                  const sAppData: nsXREAppData): Integer;
begin
  if Assigned(mainFunc) then
    Result := mainFunc(argc, argv, sAppData)
  else
    Result := 0;
end;

function XRE_GetFileFromPath(aPath: PAnsiChar;
                             out aResult: nsILocalFile): nsresult;
begin
  if Assigned(getFileFromPathFunc) then
    Result := getFileFromPathFunc(aPath, aResult)
  else
    Result := NS_ERROR_NOT_IMPLEMENTED;
end;

function XRE_GetBinaryPath(argv0: PAnsiChar;
                           out aResult: nsILocalFile): nsresult;
begin
  if Assigned(getBinaryPathFunc) then
    Result := getBinaryPathFunc(argv0, aResult)
  else
    Result := NS_ERROR_NOT_IMPLEMENTED;
end;

procedure XRE_GetStaticComponents(out aStaticComponents: PStaticModuleInfoArray;
                                 out aComponentCount: PRUint32);
begin
  if Assigned(getStaticComponentsFunc) then
    getStaticComponentsFunc(aStaticComponents, aComponentCount);
end;

function XRE_LockProfileDirectory(aDirectory: nsILocalFile;
                                  out aLockObject: nsISupports): nsresult;
begin
  if Assigned(lockProfileDirectoryFunc) then
    Result := lockProfileDirectoryFunc(aDirectory, aLockObject)
  else
    Result := NS_ERROR_NOT_IMPLEMENTED;
end;

function XRE_InitEmbedding(aLibXulDirectory: nsILocalFile;
                           aAppDirectory: nsILocalFile;
                           aAppDirProvider: nsIDirectoryServiceProvider;
                           const aStaticComponents: PStaticModuleInfoArray;
                           aStaticComponentCount: PRUint32): nsresult;
begin
  if Assigned(initEmbeddingFunc) then
    Result := initEmbeddingFunc(aLibXulDirectory,
            aAppDirectory,
            aAppDirProvider,
            aStaticComponents,
            aStaticComponentCount)
  else
  {$IFDEF XULRUNNER2}
    if Assigned(initEmbedding2Func) then
      Result := initEmbedding2Func(aLibXulDirectory,
              aAppDirectory,
              aAppDirProvider)
  else
  {$ENDIF}
    Result := NS_ERROR_NOT_IMPLEMENTED;
end;

procedure XRE_NotifyProfile();
begin
  if Assigned(notifyProfileFunc) then
    notifyProfileFunc();
end;

procedure XRE_TermEmbedding();
begin
  if Assigned(termEmbeddingFunc) then
    termEmbeddingFunc();
end;

function XRE_CreateAppData(aINIFile: nsILocalFile;
                           out aAppData: PXREAppData): nsresult;
begin
  if Assigned(createAppDataFunc) then
    Result := createAppDataFunc(aINIFile, aAppData)
  else
    Result := NS_ERROR_NOT_IMPLEMENTED;
end;

function XRE_ParseAppData(aINIFile: nsILocalFile;
                          out aAppData: nsXREAppData): nsresult;
begin
  if Assigned(parseAppDataFunc) then
    Result := parseAppDataFunc(aINIFile, aAppData)
  else
    Result := NS_ERROR_NOT_IMPLEMENTED;
end;

procedure XRE_FreeAppData(aAppData: PXREAppData);
begin
  if Assigned(freeAppDataFunc) then
    freeAppDataFunc(aAppData);
end;

end.

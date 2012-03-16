unit GeckoInit;

interface

procedure GeckoComponentsStartup(XPComPath: string = '');
procedure GeckoComponentsShutdown;

implementation

uses
  nsXPCOM, nsInit, nsTypes, nsErrorUtils, nsError,
  nsXPCOMGlue, nsXRE {$IFDEF MSWINDOWS}, Windows {$ENDIF};

var
  sInitCount: Integer = 0;

procedure GeckoComponentsStartup(XPComPath: string = '');
const
  NS_DIRECTORY_SERVICE_CID: TGUID = '{f00152d0-b40b-11d3-8c9c-000064657374}';
var
  rv: nsresult;
  errorStr: AnsiString;
  ServiceManager: nsIServiceManager;
  DirectoryService: nsIDirectoryService;
begin
  if sInitCount>0 then
  begin
    Inc(sInitCount);
    Exit;
  end;

  rv := XRE_Startup('1.9', True, '2.0', False, XPComPath);

  if NS_FAILED(rv) then
  begin
    errorStr := NS_GetErrorStringBundleKey(rv);
    XPCOMGlueShutdown;
    raise EGeckoError.Create(string(errorStr));
  end;

  //Register the service via Service Manager.
  if not Assigned(GeckoEngineDirectoryService) then
    GeckoEngineDirectoryService:=IDirectoryServiceProvider.Create;
  NS_GetServiceManager(ServiceManager);
  ServiceManager.GetService(NS_DIRECTORY_SERVICE_CID, DirectoryService,DirectoryService);
  {$IFDEF LCLGTK2 AND defined(cpux86_64)}
  //No register meanwhile the DirectoryService bug in Linux64 is not solved.
  {$ELSE}
  DirectoryService.RegisterProvider(GeckoEngineDirectoryService);
  {$ENDIF}

  Inc(sInitCount);
end;

procedure GeckoComponentsShutdown();
begin
  if sInitCount = 0 then
  begin
    raise EGeckoError.Create(
      'ERROR: Too many calls for GeckoComponentsShutdown then GeckoComponentsStartup');
  end;

  Dec(sInitCount);

  if sInitCount = 0 then
  begin
    XRE_Shutdown();
  end;
end;

end.


unit GeckoInit;

interface

procedure GeckoComponentsStartup(XPComPath: string = '');
procedure GeckoComponentsShutdown;

implementation

uses
  nsXPCOM, nsInit, nsGeckoStrings, nsTypes, nsConsts, nsErrorUtils, nsError,
  nsXPCOMGlue, nsXRE {$IFDEF MSWINDOWS}, Windows {$ENDIF};

var
  sInitCount: Integer = 0;

procedure GeckoComponentsStartup(XPComPath: string = '');
var
  rv: nsresult;

  errorStr: AnsiString;
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


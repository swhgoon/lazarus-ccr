{
This unit has been produced by ws_helper.
  Input unit name : "calcservice".
  This unit name  : "calcservice_imp".
  Date            : "17/08/2008 20:55:09".
}
Unit calcservice_imp;
{$IFDEF FPC} {$mode objfpc}{$H+} {$ENDIF}
Interface

Uses SysUtils, Classes, 
     base_service_intf, server_service_intf, server_service_imputils, calcservice;

Type


  { TCalcService_ServiceImp }

  TCalcService_ServiceImp=class(TBaseServiceImplementation,ICalcService)
  private
    procedure CheckSession();
  Protected
    function Add(
      const  A : integer; 
      const  B : integer
    ):integer;
    function Substract(
      const  A : integer; 
      const  B : integer
    ):integer;
  End;


  procedure RegisterCalcServiceImplementationFactory();

Implementation
uses config_objects;

procedure TCalcService_ServiceImp.CheckSession();
var
  cc : ICallContext;
  hc, i : PtrInt;
  h : THeaderBlock;
  lh : TLoginHeader;
  sh : TSessionHeader;
  ok : Boolean;
begin
  cc := GetCallContext();
  hc := cc.GetHeaderCount([hdOut,hdIn]);
  if ( hc < 1 ) then
    raise Exception.Create('No login or session header found.');
  ok := False;
  for i := 0 to Pred(hc) do begin
    h := cc.GetHeader(i);
    if h.InheritsFrom(TSessionHeader) or h.InheritsFrom(TLoginHeader) then begin
      ok := True;
      Break;
    end;
  end;
  if not ok then
    raise Exception.Create('No login or session header found.');
  if h.InheritsFrom(TSessionHeader) then begin
    if IsStrEmpty(TSessionHeader(h).SessionToken) then
      raise Exception.Create('Invalid session token.');
  end else begin
    lh := TLoginHeader(h);
    if IsStrEmpty(lh.UserName) or IsStrEmpty(lh.Password) then
      raise Exception.Create('Invalid login information.');
    sh := TSessionHeader.Create();
    try
      sh.SessionToken := Format('TokenOf_%s_and_%s',[lh.UserName,lh.Password]);
      sh.Direction := hdOut;
      cc.AddHeader(sh,True);
    except
      sh.Free();
      raise;
    end;
    lh.Understood := True;
  end;
end;

{ TCalcService_ServiceImp implementation }
function TCalcService_ServiceImp.Add(
  const  A : integer; 
  const  B : integer
):integer;
Begin
  CheckSession();
  Result := A + B;
End;

function TCalcService_ServiceImp.Substract(
  const  A : integer; 
  const  B : integer
):integer;
Begin
  CheckSession();
  Result := A - B;
End;



procedure RegisterCalcServiceImplementationFactory();
Begin
  GetServiceImplementationRegistry().Register('ICalcService',TImplementationFactory.Create(TCalcService_ServiceImp,wst_GetServiceConfigText('ICalcService')) as IServiceImplementationFactory);
End;

End.
